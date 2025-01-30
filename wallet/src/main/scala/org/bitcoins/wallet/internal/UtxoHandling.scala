package org.bitcoins.wallet.internal

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{Sink, Source}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.wallet.UtxoHandlingApi
import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.util.{BlockHashWithConfs, FutureUtil}
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.core.wallet.utxo.TxoState.*
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.SafeDatabase
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{AddressDAO, SpendingInfoDAO, TransactionDAO}
import org.bitcoins.wallet.util.WalletUtil
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.Future

/** Provides functionality related to handling UTXOs in our wallet. The most
  * notable examples of functionality here are enumerating UTXOs in the wallet
  * and importing a UTXO into the wallet for later spending.
  */
case class UtxoHandling(
    spendingInfoDAO: SpendingInfoDAO,
    transactionDAO: TransactionDAO,
    addressDAO: AddressDAO,
    chainQueryApi: ChainQueryApi)(implicit
    walletConfig: WalletAppConfig,
    system: ActorSystem)
    extends UtxoHandlingApi
    with BitcoinSLogger {
  import system.dispatcher

  private val walletCallbacks: WalletCallbacks = walletConfig.callBacks
  private val safeDatabase: SafeDatabase = spendingInfoDAO.safeDatabase

  override def clearAllUtxos(): Future[Unit] = {
    val aggregatedActions
        : DBIOAction[Unit, NoStream, Effect.Write & Effect.Transactional] =
      spendingInfoDAO.deleteAllAction().map(_ => ())

    val resultedF = safeDatabase.run(aggregatedActions)
    resultedF.failed.foreach(err =>
      logger.error(
        s"Failed to clear utxos, addresses and scripts from the database",
        err
      ))

    resultedF.map(_ => ())
  }

  override def clearAllAddresses(): Future[Unit] = {
    val action = addressDAO
      .deleteAllAction()
      .map(_ => ())
    safeDatabase
      .run(action)
      .map(_ => ())
  }

  override def findByScriptPubKey(
      scriptPubKey: ScriptPubKey
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findByScriptPubKey(scriptPubKey)
  }

  override def findByOutPoints(
      outPoints: Vector[TransactionOutPoint]
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findByOutPoints(outPoints)
  }

  override def findOutputsBeingSpent(
      tx: Transaction
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findOutputsBeingSpent(tx)
  }

  override def getConfirmedBalance(tag: AddressTag): Future[CurrencyUnit] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { allUnspent =>
      val confirmed = allUnspent.filter(_.state == ConfirmedReceived)
      confirmed.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  override def getUnconfirmedBalance(tag: AddressTag): Future[CurrencyUnit] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { allUnspent =>
      val confirmed = allUnspent
        .filter(utxo => TxoState.pendingReceivedStates.contains(utxo.state))
      confirmed.foldLeft(CurrencyUnits.zero)(_ + _.output.value)
    }
  }

  /** @inheritdoc */
  override def getUtxos(): Future[Vector[SpendingInfoDb]] = {
    getUtxos(walletConfig.defaultAccount)
  }

  def getUtxos(
      hdAccount: HDAccount
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findAllUnspentForAccount(hdAccount)
  }

  /** Returns all the utxos originating from the given outpoints */
  override def getUtxos(
      outPoints: Vector[TransactionOutPoint]
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO
      .findAllSpendingInfos()
      .map(_.filter(spendingInfo => outPoints.contains(spendingInfo.outPoint)))
  }

  override def getUtxos(tag: AddressTag): Future[Vector[SpendingInfoDb]] = {
    getUtxos(walletConfig.defaultAccount, tag)
  }

  override def getUtxos(
      hdAccount: HDAccount,
      tag: AddressTag
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { utxos =>
      utxos.filter(utxo =>
        HDAccount
          .isSameAccount(bip32Path = utxo.privKeyPath, account = hdAccount))
    }
  }

  override def getUtxos(state: TxoState): Future[Vector[SpendingInfoDb]] = {
    getUtxos(walletConfig.defaultAccount, state)
  }

  override def getUtxos(
      hdAccount: HDAccount,
      state: TxoState
  ): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findByTxoState(state).map { utxos =>
      utxos.filter(utxo =>
        HDAccount
          .isSameAccount(bip32Path = utxo.privKeyPath, account = hdAccount))
    }
  }

  private[wallet] def updateUtxoSpentConfirmedStates(
      relevantBlocks: Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Read & Effect.Write] = {
    updateUtxoStates(relevantBlocks, UtxoHandling.updateSpentTxoWithConfs)
  }

  private[wallet] def updateUtxoReceiveConfirmedStates(
      relevantBlocks: Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Read & Effect.Write] = {
    updateUtxoStates(relevantBlocks, UtxoHandling.updateReceivedTxoWithConfs)
  }

  /** Returns a map of the SpendingInfoDbs with their relevant block. If the
    * block hash is None, then it is a mempool transaction. The relevant block
    * is determined by if the utxo has been spent or not. If it has been spent
    * it uses the block that included the spending transaction, otherwise it
    * uses the block that included the receiving transaction.
    */
  private[wallet] def getDbsByRelevantBlock(
      spendingInfoDbs: Vector[SpendingInfoDb]
  ): Future[Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]]] = {
    val txIds =
      spendingInfoDbs.map { db =>
        db.spendingTxIdOpt match {
          case Some(spendingTxId) =>
            spendingTxId
          case None =>
            db.txid
        }
      }

    val blockHashMapF = transactionDAO.findByTxIdBEs(txIds).map { txDbs =>
      val blockHashMap = txDbs.map(db => db.txIdBE -> db.blockHashOpt).toMap
      val blockHashAndDb = spendingInfoDbs.map { txo =>
        val txToUse = txo.state match {
          case _: ReceivedState | ImmatureCoinbase | Reserved |
              BroadcastReceived =>
            txo.txid
          case PendingConfirmationsSpent | ConfirmedSpent | BroadcastSpent =>
            txo.spendingTxIdOpt.get
        }
        (blockHashMap(txToUse), txo)
      }
      blockHashAndDb.groupBy(_._1).map { case (blockHashOpt, vec) =>
        blockHashOpt -> vec.map(_._2)
      }
    }

    blockHashMapF.flatMap(getConfirmationsForBlocks)
  }

  /** Updates all the given SpendingInfoDbs to the correct state based on how
    * many confirmations they have received
    * @param spendingInfoDbs
    *   the utxos we need to update
    * @param fn
    *   the function used to transition the [[TxoState]] given a utxo and number
    *   of confirmations
    */
  private def updateUtxoStates(
      txsByBlock: Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]],
      fn: (SpendingInfoDb, Int, Int) => SpendingInfoDb
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Read & Effect.Write] = {
    val toUpdates: Vector[SpendingInfoDb] = txsByBlock.flatMap {
      case (Some(blockHashWithConfs), txos) =>
        blockHashWithConfs.confirmationsOpt match {
          case None =>
            logger.warn(
              s"Given txos exist in block (${blockHashWithConfs.blockHash.hex}) that we do not have or that has been reorged! $txos"
            )
            Vector.empty
          case Some(confs) =>
            txos.map(fn(_, confs, walletConfig.requiredConfirmations))
        }
      case (None, txos) =>
        if (txos.nonEmpty) {
          logger.debug(
            s"Currently have ${txos.size} transactions in the mempool"
          )
        }
        txos
    }.toVector
    if (toUpdates.nonEmpty)
      logger.info(s"${toUpdates.size} txos are now confirmed!")
    else logger.trace("No txos to be confirmed")
    spendingInfoDAO.upsertAllSpendingInfoDbAction(toUpdates)
  }

  /** Fetches confirmations for the given blocks in parallel */
  private def getConfirmationsForBlocks(
      relevantBlocks: Map[Option[DoubleSha256DigestBE], Vector[SpendingInfoDb]]
  ): Future[Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]]] = {

    val resultF = Source(relevantBlocks)
      .mapAsync(FutureUtil.getParallelism) {
        case (blockHashOpt, spendingInfoDbs) =>
          blockHashOpt match {
            case Some(blockHash) =>
              WalletUtil
                .getBlockHashWithConfs(chainQueryApi, blockHash)
                .map(blockWithConfsOpt => (blockWithConfsOpt, spendingInfoDbs))
            case None =>
              Future.successful((None, spendingInfoDbs))
          }
      }
      .runWith(Sink.seq)

    resultF.map(_.toMap)
  }

  /** Constructs a DB level representation of the given UTXO, and persist it to
    * disk
    */
  private def writeUtxo(
      tx: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb
  ): DBIOAction[SpendingInfoDb, NoStream, Effect.Read & Effect.Write] = {
    val confs: Int = blockHashWithConfsOpt
      .flatMap(_.confirmationsOpt)
      .getOrElse(0)

    val state: TxoState = {
      if (
        tx.inputs.head
          .isInstanceOf[CoinbaseInput] && confs <= Consensus.coinbaseMaturity
      ) {
        TxoState.ImmatureCoinbase
      } else {
        UtxoHandling.getReceiveConfsState(
          confs,
          walletConfig.requiredConfirmations
        )
      }
    }

    val utxo: SpendingInfoDb =
      addressDb match {
        case taprootAddr: TaprootAddressDb =>
          TaprootSpendingInfo(
            outPoint = outPoint,
            output = output,
            privKeyPath = taprootAddr.path,
            state = state,
            spendingTxIdOpt = None,
            id = None
          )
        case segwitAddr: SegWitAddressDb =>
          SegwitV0SpendingInfo(
            state = state,
            outPoint = outPoint,
            output = output,
            privKeyPath = segwitAddr.path,
            scriptWitness = segwitAddr.witnessScript,
            spendingTxIdOpt = None
          )
        case LegacyAddressDb(path, _, _, _, _) =>
          LegacySpendingInfo(
            state = state,
            outPoint = outPoint,
            output = output,
            privKeyPath = path,
            spendingTxIdOpt = None
          )
        case nested: NestedSegWitAddressDb =>
          NestedSegwitV0SpendingInfo(
            outPoint = outPoint,
            output = output,
            privKeyPath = nested.path,
            redeemScript = P2WPKHWitnessSPKV0(nested.ecPublicKey),
            scriptWitness = P2WPKHWitnessV0(nested.ecPublicKey),
            state = state,
            spendingTxIdOpt = None,
            id = None
          )
      }

    for {
      written <- spendingInfoDAO.createUnlessAction(utxo) {
        (foundUtxo, utxoToCreate) =>
          foundUtxo.state.isInstanceOf[SpentState] && utxoToCreate.state
            .isInstanceOf[ReceivedState]
      }
    } yield {
      val writtenOut = written.outPoint
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txIdBE.hex}:${writtenOut.vout.toInt} amt=${output.value} into DB"
      )
      logger.debug(s"UTXO details: ${written.output}")
      written
    }
  }

  override def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]
  ): Future[Vector[SpendingInfoDb]] = {
    for {
      (utxos, callbackF) <- safeDatabase.run(markUTXOsAsReservedAction(utxos))
      _ <- callbackF
    } yield utxos
  }

  def markUTXOsAsReservedAction(
      utxos: Vector[SpendingInfoDb]
  ): DBIOAction[
    (Vector[SpendingInfoDb], Future[Unit]),
    NoStream,
    Effect.Read & Effect.Write
  ] = {
    val outPoints = utxos.map(_.outPoint)
    logger.info(s"Reserving utxos=$outPoints")
    val updated = utxos.map(_.copyWithState(TxoState.Reserved))
    spendingInfoDAO.markAsReservedAction(updated).map { utxos =>
      val callbackF = walletCallbacks.executeOnReservedUtxos(utxos)
      (utxos, callbackF)
    }
  }

  /** @inheritdoc */
  override def markUTXOsAsReserved(
      tx: Transaction
  ): Future[Vector[SpendingInfoDb]] = {
    for {
      utxos <- spendingInfoDAO.findOutputsBeingSpent(tx)
      reserved <- markUTXOsAsReserved(utxos)
    } yield reserved
  }

  override def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]
  ): Future[Vector[SpendingInfoDb]] = {
    logger.info(s"Unreserving utxos ${utxos.map(_.outPoint)}")
    val updatedUtxosF
        : Future[Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]]] = {
      // make sure exception isn't thrown outside of a future to fix
      // see: https://github.com/bitcoin-s/bitcoin-s/issues/3813
      val unreserved = utxos.filterNot(_.state == TxoState.Reserved)
      if (unreserved.nonEmpty) {
        val exn = new IllegalArgumentException(
          s"Some utxos are not reserved, got unreserved=$unreserved utxos=$utxos")
        Future.failed(exn)
      } else {
        // unmark all utxos are reserved
        val updatedUtxos = utxos
          .map(_.copyWithState(TxoState.PendingConfirmationsReceived))
        getDbsByRelevantBlock(updatedUtxos)
      }
    }

    val updatedActionF: Future[DBIOAction[Vector[SpendingInfoDb],
                                          NoStream,
                                          Effect.Read & Effect.Write]] = {
      updatedUtxosF.map { updatedUtxos =>
        for {
          // update the confirmed utxos
          updatedConfirmed <- updateUtxoReceiveConfirmedStates(updatedUtxos)

          // update the utxos that are in blocks but not considered confirmed yet
          pendingConf =
            updatedUtxos.values.flatten
              .filterNot(utxo =>
                updatedConfirmed.exists(_.outPoint == utxo.outPoint))
          updated <-
            spendingInfoDAO.updateAllSpendingInfoDbAction(
              (pendingConf ++ updatedConfirmed).toVector)
        } yield updated
      }
    }

    val runF = updatedActionF.flatMap(safeDatabase.run)
    runF.flatMap { vec =>
      walletCallbacks
        .executeOnReservedUtxos(vec)
        .map(_ => vec)
    }
  }

  /** @inheritdoc */
  override def unmarkUTXOsAsReserved(
      tx: Transaction
  ): Future[Vector[SpendingInfoDb]] = {
    for {
      utxos <- spendingInfoDAO.findOutputsBeingSpent(tx)
      reserved = utxos.filter(_.state == TxoState.Reserved)
      updated <- unmarkUTXOsAsReserved(reserved.toVector)
    } yield updated
  }

  /** @inheritdoc */
  override def updateUtxoPendingStates(): Future[Vector[SpendingInfoDb]] = {
    for {
      infos <- spendingInfoDAO.findAllPendingConfirmation
      _ = logger.debug(s"Updating states of ${infos.size} pending utxos...")
      receivedUtxos = infos.filter(_.state.isInstanceOf[ReceivedState])
      spentUtxos = infos.filter(_.state.isInstanceOf[SpentState])
      receivedWithBlocks <- getDbsByRelevantBlock(receivedUtxos)
      spentWithBlocks <- getDbsByRelevantBlock(spentUtxos)
      updatedReceivedInfosA = updateUtxoReceiveConfirmedStates(
        receivedWithBlocks)
      updatedReceivedInfos <- safeDatabase.run(updatedReceivedInfosA)
      updatedSpentInfosA = updateUtxoSpentConfirmedStates(spentWithBlocks)
      updatedSpentInfos <- safeDatabase.run(updatedSpentInfosA)
    } yield (updatedReceivedInfos ++ updatedSpentInfos)
  }

  /** Inserts the UTXO at the given index into our DB, swallowing the error if
    * any (this is because we're operating on data we've already verified).
    */
  def processReceivedUtxoAction(
      transaction: Transaction,
      index: Int,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      addressDb: AddressDb
  ): DBIOAction[SpendingInfoDb, NoStream, Effect.Read & Effect.Write] = {
    val output = transaction.outputs(index)
    val outPoint = TransactionOutPoint(transaction.txId, UInt32(index))

    // insert the UTXO into the DB
    val utxoF = writeUtxo(
      tx = transaction,
      blockHashWithConfsOpt = blockHashWithConfsOpt,
      output = output,
      outPoint = outPoint,
      addressDb = addressDb
    )
    utxoF
  }

  /** Get total wallet balance for the default HD account */
  override def getBalance(): Future[CurrencyUnit] = {
    getBalance(walletConfig.defaultAccount)
  }

  /** Get total confirmed wallet balance for the default HD account */
  override def getConfirmedBalance(): Future[CurrencyUnit] = {
    getConfirmedBalance(walletConfig.defaultAccount)
  }

  /** Get total unconfirmed wallet balance for the default HD account */
  override def getUnconfirmedBalance(): Future[CurrencyUnit] = {
    getUnconfirmedBalance(walletConfig.defaultAccount)
  }

  override def getBalance(account: HDAccount): Future[CurrencyUnit] = {
    val action = spendingInfoDAO.getBalanceAction(accountOpt = Some(account))
    safeDatabase.run(action)
  }

  override def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit] = {
    val action =
      spendingInfoDAO.getConfirmedBalanceAction(accountOpt = Some(account))
    safeDatabase.run(action)
  }

  override def getUnconfirmedBalance(
      account: HDAccount): Future[CurrencyUnit] = {
    val action =
      spendingInfoDAO.getUnconfirmedBalanceAction(accountOpt = Some(account))
    safeDatabase.run(action)
  }
}

object UtxoHandling {

  /** Updates the SpendingInfoDb to the correct state based on the number of
    * confirmations it has received
    */
  def updateReceivedTxoWithConfs(
      txo: SpendingInfoDb,
      confs: Int,
      requiredConfirmations: Int
  ): SpendingInfoDb = {
    txo.state match {
      case TxoState.ImmatureCoinbase =>
        if (confs > Consensus.coinbaseMaturity) {
          if (confs >= requiredConfirmations)
            txo.copyWithState(TxoState.ConfirmedReceived)
          else
            txo.copyWithState(TxoState.PendingConfirmationsReceived)
        } else txo
      case TxoState.PendingConfirmationsReceived | BroadcastReceived |
          TxoState.ConfirmedReceived =>
        val state = getReceiveConfsState(confs, requiredConfirmations)
        txo.copyWithState(state)
      case TxoState.Reserved =>
        // do nothing if we have reserved the utxo
        txo
      case state: SpentState =>
        sys.error(s"Cannot update spendingInfoDb in spent state=$state")

    }
  }

  /** Given a number of confirmations and the required confirmations for the
    * wallet this method returns the appropriate [[ReceivedState]] for the
    * number of confirmations
    */
  def getReceiveConfsState(
      confs: Int,
      requireConfirmations: Int
  ): ReceivedState = {
    if (confs < 0) {
      sys.error(
        s"Cannot have negative confirmations, got=$confs. Did the block get reorged or exist?"
      )
    } else if (confs == 0) {
      TxoState.BroadcastReceived
    } else if (confs >= requireConfirmations) {
      TxoState.ConfirmedReceived
    } else {
      TxoState.PendingConfirmationsReceived
    }
  }

  def updateSpentTxoWithConfs(
      txo: SpendingInfoDb,
      confs: Int,
      requiredConfirmations: Int
  ): SpendingInfoDb = {
    txo.state match {
      case TxoState.ImmatureCoinbase =>
        sys.error(
          s"Cannot update txo with received state=${TxoState.ImmatureCoinbase}"
        )
      case TxoState.Reserved | TxoState.PendingConfirmationsSpent |
          TxoState.ConfirmedSpent | TxoState.BroadcastSpent |
          TxoState.PendingConfirmationsReceived | TxoState.BroadcastReceived |
          TxoState.ConfirmedReceived =>
        if (confs >= requiredConfirmations) {
          txo.copyWithState(TxoState.ConfirmedSpent)
        } else if (confs == 0) {
          txo.copyWithState(TxoState.BroadcastSpent)
        } else {
          txo.copyWithState(TxoState.PendingConfirmationsSpent)
        }
    }
  }
}
