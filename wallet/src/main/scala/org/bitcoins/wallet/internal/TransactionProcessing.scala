package org.bitcoins.wallet.internal

import org.apache.pekko.actor.{ActorRef, Status}
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Sink, Source}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.wallet.{
  ProcessTxResult,
  TransactionProcessingApi,
  WalletApi
}
import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  OutputWithIndex,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.util.{BlockHashWithConfs, TimeUtil}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.TxoState.*
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.SafeDatabase
import org.bitcoins.wallet.*
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AddressDAO,
  AddressTagDAO,
  IncomingTransactionDAO,
  OutgoingTransactionDAO,
  ScriptPubKeyDAO,
  SpendingInfoDAO,
  TransactionDAO,
  WalletDAOs,
  WalletStateDescriptorDAO
}
import org.bitcoins.wallet.util.WalletUtil
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

/** Provides functionality for processing transactions. This includes importing
  * UTXOs spent to our wallet, updating confirmation counts and marking UTXOs as
  * spent when spending from our wallet
  */
case class TransactionProcessing(
    walletApi: WalletApi,
    chainQueryApi: ChainQueryApi,
    utxoHandling: UtxoHandling,
    walletDAOs: WalletDAOs)(implicit
    walletConfig: WalletAppConfig,
    ec: ExecutionContext)
    extends TransactionProcessingApi
    with WalletLogger {
  import org.bitcoins.core.currency.currencyUnitNumeric
  private def walletCallbacks: WalletCallbacks = walletConfig.callBacks
  private val stateDescriptorDAO: WalletStateDescriptorDAO =
    walletDAOs.stateDescriptorDAO
  private val spendingInfoDAO: SpendingInfoDAO = walletDAOs.utxoDAO
  private val transactionDAO: TransactionDAO = walletDAOs.transactionDAO
  private val scriptPubKeyDAO: ScriptPubKeyDAO = walletDAOs.scriptPubKeyDAO
  private val addressDAO: AddressDAO = walletDAOs.addressDAO
  private val incomingTxDAO: IncomingTransactionDAO = walletDAOs.incomingTxDAO
  private val outgoingTxDAO: OutgoingTransactionDAO = walletDAOs.outgoingTxDAO
  private val addressTagDAO: AddressTagDAO = walletDAOs.addressTagDAO
  private val safeDatabase: SafeDatabase = stateDescriptorDAO.safeDatabase
  private val networkParameters: NetworkParameters = walletConfig.network

  /////////////////////
  // Public facing API
  override def findByTxIds(
      txIds: Vector[DoubleSha256DigestBE]
  ): Future[Vector[TransactionDb]] = {
    transactionDAO.findByTxIds(txIds)
  }

  /** @inheritdoc */
  override def processTransaction(
      transaction: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs]
  ): Future[Unit] = {
    for {
      relevantReceivedOutputs <- getRelevantOutputs(transaction)
      action = processTransactionImpl(
        transaction = transaction,
        blockHashWithConfsOpt = blockHashWithConfsOpt,
        newTags = Vector.empty,
        receivedSpendingInfoDbsOpt = None,
        spentSpendingInfoDbsOpt = None,
        relevantReceivedOutputs = relevantReceivedOutputs
      )
      processTx <- safeDatabase.run(action)
      // only notify about our transactions
      _ <-
        if (
          processTx.updatedIncoming.nonEmpty || processTx.updatedOutgoing.nonEmpty
        ) {
          logger.info(
            s"Finished processing of transaction=${transaction.txIdBE.hex}. Relevant incomingTXOs=${processTx.updatedIncoming.length}, outgoingTXOs=${processTx.updatedOutgoing.length}"
          )
          walletCallbacks.executeOnTransactionProcessed(transaction)
        } else Future.unit
    } yield {
      ()
    }
  }

  override def processBlock(block: Block): Future[Unit] = {
    val start = TimeUtil.currentEpochMs
    val isEmptyF = walletApi.isEmpty()
    val heightF = chainQueryApi.getBlockHeight(block.blockHeader.hashBE)
    heightF.foreach { heightOpt =>
      logger.debug(
        s"Processing block=${block.blockHeader.hashBE.hex} heightOpt=$heightOpt"
      )
    }

    heightF.flatMap {
      case Some(height) =>
        val resF = for {
          isEmpty <- isEmptyF
          _ <- {
            if (!isEmpty) {
              processBlockCachedUtxos(block)
            } else {
              // do nothing if the wallet is empty as an optimization
              // this is for users first downloading bitcoin-s
              // and syncing their node
              Future.successful(this)
            }
          }
        } yield ()

        val f = for {
          _ <- resF
          hash = block.blockHeader.hashBE
          _ <- stateDescriptorDAO.updateSyncHeight(hash, height)
          _ <- walletConfig.callBacks.executeOnBlockProcessed(block)
        } yield {
          ()
        }

        f.onComplete(failure =>
          signalBlockProcessingCompletion(block.blockHeader.hashBE, failure))

        f.foreach { _ =>
          val stop = TimeUtil.currentEpochMs
          logger.info(
            s"Finished processing of block=${block.blockHeader.hash.flip.hex}. It took ${stop - start}ms"
          )
        }
        f.failed.foreach(e =>
          logger.error(
            s"Error processing of block=${block.blockHeader.hash.flip.hex}.",
            e
          ))
        f
      case None =>
        logger.warn(
          s"Could not find blockheight for blockHash=${block.blockHeader.hashBE.hex}, skipping processing in wallet"
        )
        Future.unit
    }

  }

  /** Helper method to process a block. This fetches all of our relevent
    * spending info dbs up front rather than fetching them every time
    * [[processTransaction]] is called. This significantly improves performance
    * on rescans or IBD with an existing wallet
    */
  private def processBlockCachedUtxos(block: Block): Future[Unit] = {
    // fetch all received spending info dbs relevant to txs in this block to improve performance
    val receivedSpendingInfoDbsF =
      spendingInfoDAO
        .findTxs(block.transactions)

    val cachedReceivedOptF = receivedSpendingInfoDbsF
      .map(Some(_)) // reduce allocations by creating Some here

    val blockHash = block.blockHeader.hashBE
    val blockHashWithConfsOptF: Future[Option[BlockHashWithConfs]] =
      WalletUtil.getBlockHashWithConfs(chainQueryApi, blockHash)

    // fetch all outputs we may have received in this block in advance
    // as an optimization
    val relevantReceivedOutputsForBlockF = getRelevantOutputsForBlock(block)

    val actionsF: Future[Vector[
      DBIOAction[ProcessTxResult, NoStream, Effect.Read & Effect.Write]]] =
      for {
        // map on these first so we don't have to call
        // .map everytime we iterate through a tx
        // which is costly (thread overhead)
        receivedSpendingInfoDbsOpt <- cachedReceivedOptF
        relevantReceivedOutputsForBlock <-
          relevantReceivedOutputsForBlockF
        blockHashWithConfsOpt <- blockHashWithConfsOptF
        actions = {
          block.transactions.map { transaction =>
            val relevantReceivedOutputsForTx = relevantReceivedOutputsForBlock
              .getOrElse(transaction.txIdBE, Vector.empty)
            for {
              action <-
                processTransactionImpl(
                  transaction = transaction,
                  blockHashWithConfsOpt = blockHashWithConfsOpt,
                  newTags = Vector.empty,
                  receivedSpendingInfoDbsOpt = receivedSpendingInfoDbsOpt,
                  spentSpendingInfoDbsOpt = None,
                  relevantReceivedOutputs = relevantReceivedOutputsForTx
                )
            } yield {
              action
            }
          }
        }
      } yield {
        actions
      }

    actionsF
      .flatMap(actions => safeDatabase.run(DBIOAction.sequence(actions)))
      .map(_ => ())
  }

  override def findTransaction(
      txId: DoubleSha256DigestBE
  ): Future[Option[TransactionDb]] = {
    transactionDAO.findByTxId(txId)
  }

  override def listTransactions(): Future[Vector[TransactionDb]] =
    transactionDAO.findAll()

  /////////////////////
  // Internal wallet API

  override def insertTransaction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[TransactionDb] = {
    safeDatabase.run(insertTransactionAction(tx, blockHashOpt))
  }

  protected def insertTransactionAction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): DBIOAction[TransactionDb, NoStream, Effect.Write & Effect.Read] = {
    val txDb = TransactionDbHelper.fromTransaction(tx, blockHashOpt)
    transactionDAO.upsertAction(txDb)
  }

  private[wallet] def insertOutgoingTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[(TransactionDb, OutgoingTransactionDb)] = {
    val outgoingDb =
      OutgoingTransactionDb.fromTransaction(
        transaction,
        inputAmount,
        sentAmount,
        feeRate.calc(transaction)
      )
    for {
      txDb <- insertTransaction(transaction, blockHashOpt)
      written <- outgoingTxDAO.upsert(outgoingDb)
    } yield (txDb, written)
  }

  /** Processes TXs originating from our wallet. This is called right after
    * we've signed a TX, updating our UTXO state.
    */
  override def processOurTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      newTags: Vector[AddressTag]
  ): Future[ProcessTxResult] = {
    logger.info(
      s"Processing TX from our wallet, transaction=${transaction.txIdBE.hex} with blockHash=${blockHashWithConfsOpt
          .map(_.blockHash.hex)}"
    )
    val relevantOutputsF = getRelevantOutputs(transaction)
    for {
      (txDb, _) <-
        insertOutgoingTransaction(
          transaction,
          feeRate,
          inputAmount,
          sentAmount,
          blockHashWithConfsOpt.map(_.blockHash)
        )
      relevantOutputs <- relevantOutputsF
      action = processTransactionImpl(
        transaction = txDb.transaction,
        blockHashWithConfsOpt = blockHashWithConfsOpt,
        newTags = newTags,
        receivedSpendingInfoDbsOpt = None,
        spentSpendingInfoDbsOpt = None,
        relevantOutputs
      )
      result <- safeDatabase.run(action)
      _ <-
        if (
          result.updatedIncoming.nonEmpty || result.updatedOutgoing.nonEmpty
        ) {
          walletCallbacks.executeOnTransactionProcessed(transaction)
        } else Future.unit
    } yield {
      val txid = txDb.transaction.txIdBE
      val changeOutputs = result.updatedIncoming.length
      val spentOutputs = result.updatedOutgoing.length

      logger.info(
        s"Processing of internal transaction=${txid.hex} resulted in changeOutputs=$changeOutputs and spentUTXOs=$spentOutputs"
      )
      result
    }
  }

  override def subscribeForBlockProcessingCompletionSignal(
      blockHash: DoubleSha256DigestBE
  ): Future[DoubleSha256DigestBE] = {
    import walletConfig.materializer

    val p = Promise[DoubleSha256DigestBE]()
    val actor: ActorRef = Source
      .actorRef[DoubleSha256DigestBE](completionMatcher = PartialFunction.empty,
                                      failureMatcher = PartialFunction.empty,
                                      bufferSize = 1,
                                      overflowStrategy =
                                        OverflowStrategy.dropHead)
      .to(Sink.foreach(event => p.trySuccess(event)))
      .run()

    val _: Boolean = walletConfig.system.eventStream
      .subscribe(actor, classOf[DoubleSha256DigestBE])

    p.future.onComplete {
      case scala.util.Success(b) =>
        val msg = Status
          .Success(s"Successfully received blockHash=$b")
        actor.tell(msg, ActorRef.noSender)
      case scala.util.Failure(err) =>
        val msg = Status.Failure(err)
        actor.tell(msg, ActorRef.noSender)
    }

    p.future
  }

  private def signalBlockProcessingCompletion(
      blockHash: DoubleSha256DigestBE,
      failure: Try[?]
  ): Unit = {
    failure.failed.foreach(err =>
      logger.error(s"Failed to fetch block=$blockHash", err))
    walletConfig.system.eventStream.publish(blockHash)
  }

  private def processReceivedUtxosAction(
      transaction: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      spendingInfoDbs: Vector[SpendingInfoDb],
      newTags: Vector[AddressTag],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Read & Effect.Write] = {
    if (spendingInfoDbs.isEmpty && relevantReceivedOutputs.isEmpty) {
      // as an optimization if we don't have any relevant utxos
      // and any relevant outputs that match scripts in our wallet
      // we can just return now
      DBIOAction.successful(Vector.empty)
    } else {
      val newOutputs = relevantReceivedOutputs.filterNot { output =>
        val outPoint =
          TransactionOutPoint(transaction.txId, UInt32(output.index))
        spendingInfoDbs.exists(_.outPoint == outPoint)
      }
      if (newOutputs.nonEmpty) {
        processNewReceivedTx(transaction,
                             blockHashWithConfirmations = blockHashWithConfsOpt,
                             newTags,
                             newOutputs)
          .map(_.toVector)
      } else {
        val processedVec = spendingInfoDbs.map { txo =>
          processExistingReceivedTxo(transaction, blockHashWithConfsOpt, txo)
        }
        slick.dbio.DBIOAction.sequence(processedVec)
      }
    }
  }

  /** Processes received utxos that are contained in the given transaction
    * @param transaction
    *   the transaction that we are receiving utxos from
    * @param blockHashOpt
    *   the block hash that contains this tx
    * @param spendingInfoDbs
    *   the spending info dbs that are relevant for this transaction
    * @param newTags
    *   tags associated with this tx
    * @return
    */
  override def processReceivedUtxos(
      transaction: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      spendingInfoDbs: Vector[SpendingInfoDb],
      newTags: Vector[AddressTag],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): Future[Vector[SpendingInfoDb]] = {

    val action = processReceivedUtxosAction(transaction,
                                            blockHashWithConfsOpt,
                                            spendingInfoDbs,
                                            newTags,
                                            relevantReceivedOutputs)
    safeDatabase.run(action)
  }

  override def processSpentUtxos(
      transaction: Transaction,
      outputsBeingSpent: Vector[SpendingInfoDb],
      blockHashWithConfsOpt: Option[BlockHashWithConfs]
  ): Future[Vector[SpendingInfoDb]] = {
    val action = processSpentUtxosAction(transaction,
                                         outputsBeingSpent,
                                         blockHashWithConfsOpt)

    safeDatabase.run(action)
  }

  /** Searches for outputs on the given transaction that are being spent from
    * our wallet
    */
  private def processSpentUtxosAction(
      transaction: Transaction,
      outputsBeingSpent: Vector[SpendingInfoDb],
      blockHashWithConfs: Option[BlockHashWithConfs]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Read & Effect.Write] = {
    for {
      _ <- {
        if (outputsBeingSpent.nonEmpty)
          insertTransactionAction(transaction,
                                  blockHashWithConfs.map(_.blockHash))
        else DBIOAction.unit
      }
      toBeUpdated = outputsBeingSpent.flatMap(
        markAsSpent(_, transaction.txIdBE)
      )
      m = Map(blockHashWithConfs -> toBeUpdated)
      processed <- utxoHandling.updateUtxoSpentConfirmedStates(m)
    } yield {
      processed
    }
  }

  /** Does the grunt work of processing a TX. This is called by either the
    * internal or public TX processing method, which logs and transforms the
    * output fittingly.
    */
  private[internal] def processTransactionImpl(
      transaction: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      newTags: Vector[AddressTag],
      receivedSpendingInfoDbsOpt: Option[Vector[SpendingInfoDb]],
      spentSpendingInfoDbsOpt: Option[Vector[SpendingInfoDb]],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): DBIOAction[ProcessTxResult, NoStream, Effect.Read & Effect.Write] = {

    logger.debug(
      s"Processing transaction=${transaction.txIdBE.hex} with blockHash=${blockHashWithConfsOpt
          .map(_.blockHash.hex)}"
    )
    val receivedSpendingInfoDbsA
        : DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
      receivedSpendingInfoDbsOpt match {
        case Some(received) =>
          // spending info dbs are cached, so fetch the one relevant for this tx
          val filtered = received.filter(_.txid == transaction.txIdBE)
          DBIOAction.successful(filtered)
        case None =>
          // no caching, just fetch from the database
          spendingInfoDAO.findTxAction(transaction)
      }

    }

    val spentSpendingInfoDbsF
        : DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
      spentSpendingInfoDbsOpt match {
        case Some(spent) =>
          // spending info dbs are cached, so filter for outpoints related to this tx
          val filtered = spent.filter { s =>
            transaction.inputs.exists(_.previousOutput == s.outPoint)
          }
          DBIOAction.successful(filtered)
        case None =>
          // no caching, just fetch from db
          spendingInfoDAO.findOutputsBeingSpentAction(Vector(transaction))
      }
    }
    val action
        : DBIOAction[ProcessTxResult, NoStream, Effect.Write & Effect.Read] =
      for {
        receivedSpendingInfoDbs <- receivedSpendingInfoDbsA
        receivedStart = TimeUtil.currentEpochMs
        incoming <- processReceivedUtxosAction(
          transaction = transaction,
          blockHashWithConfsOpt = blockHashWithConfsOpt,
          spendingInfoDbs = receivedSpendingInfoDbs,
          newTags = newTags,
          relevantReceivedOutputs = relevantReceivedOutputs
        )
        _ = if (incoming.nonEmpty) {
          logger.info(
            s"Finished processing ${incoming.length} received outputs, balance=${incoming
                .map(_.output.value)
                .sum} it took=${TimeUtil.currentEpochMs - receivedStart}ms"
          )
        }

        spentSpendingInfoDbs <- spentSpendingInfoDbsF
        spentStart = TimeUtil.currentEpochMs
        outgoing <- processSpentUtxosAction(
          transaction = transaction,
          outputsBeingSpent = spentSpendingInfoDbs,
          blockHashWithConfs = blockHashWithConfsOpt
        )
        _ = if (outgoing.nonEmpty) {
          logger.info(
            s"Finished processing ${outgoing.length} spent outputs, it took=${TimeUtil.currentEpochMs - spentStart}ms"
          )
        }
      } yield {
        ProcessTxResult(incoming, outgoing)
      }

    action

  }

  /** If the given UTXO is marked as unspent and returns it so it can be updated
    * Otherwise returns None.
    *
    * If the utxo is transitioning into an invalid state it throws ane
    * exception.
    */
  private def markAsSpent(
      out: SpendingInfoDb,
      spendingTxId: DoubleSha256DigestBE
  ): Option[SpendingInfoDb] = {
    out.state match {
      case ConfirmedReceived | PendingConfirmationsReceived |
          BroadcastReceived =>
        val updated =
          out
            .copyWithSpendingTxId(spendingTxId)
            .copyWithState(state = BroadcastSpent)
        logger.debug(
          s"Marked utxo=${updated.toHumanReadableString} as state=${updated.state}"
        )
        Some(updated)
      case TxoState.Reserved =>
        val updated =
          out
            .copyWithSpendingTxId(spendingTxId)
            .copyWithState(state = BroadcastSpent)
        Some(updated)
      case TxoState.BroadcastSpent =>
        if (!out.spendingTxIdOpt.contains(spendingTxId)) {
          logger.warn(
            s"Updating the spendingTxId of a transaction that is already spent, " +
              s"old state=${TxoState.BroadcastSpent} old spendingTxId=${out.spendingTxIdOpt
                  .map(_.hex)} new spendingTxId=${spendingTxId.hex}"
          )
        }
        val updated =
          out.copyWithSpendingTxId(spendingTxId)
        Some(updated)
      case TxoState.ImmatureCoinbase =>
        throw new RuntimeException(
          s"Attempting to spend an ImmatureCoinbase ${out.outPoint.hex}, this should not be possible until it is confirmed."
        )
      case TxoState.ConfirmedSpent | TxoState.PendingConfirmationsSpent =>
        if (!out.spendingTxIdOpt.contains(spendingTxId)) {
          throw new RuntimeException(
            s"Attempted to mark an already spent utxo ${out.outPoint.hex} with a new spending tx ${spendingTxId.hex}"
          )
        } else {
          // do not want to update again
          None
        }
    }
  }

  /** Processes an incoming transaction that already exists in our wallet. If
    * the incoming transaction has more confirmations than what we have in the
    * DB, we update the TX
    */
  private def processExistingReceivedTxo(
      transaction: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      foundTxo: SpendingInfoDb
  ): DBIOAction[SpendingInfoDb, NoStream, Effect.Read & Effect.Write] = {
    if (foundTxo.txid != transaction.txIdBE) {
      val errMsg =
        Seq(
          s"Found TXO has txid=${foundTxo.txid}, tx we were given has txid=${transaction.txIdBE}.",
          "This is either a reorg or a double spent, which is not implemented yet"
        ).mkString(" ")
      logger.error(errMsg)
      slick.dbio.DBIOAction.failed(new RuntimeException(errMsg))
    } else {
      blockHashWithConfsOpt match {
        case Some(blockHashWithConfs) =>
          val blockHash = blockHashWithConfs.blockHash
          logger.debug(
            s"Updating block_hash of txo=${transaction.txIdBE.hex}, new block hash=${blockHash.hex}"
          )
          val updateTxDbA =
            insertTransactionAction(transaction, Some(blockHash))

          // Update Txo State
          val map = Map(blockHashWithConfsOpt -> Vector(foundTxo))
          updateTxDbA.flatMap(_ =>
            utxoHandling.updateUtxoReceiveConfirmedStates(map).flatMap { vec =>
              vec.headOption match {
                case Some(txo) =>
                  logger.debug(
                    s"Updated block_hash of txo=${txo.txid.hex} new block hash=${blockHash.hex}"
                  )
                  slick.dbio.DBIOAction.successful(txo)
                case None =>
                  // State was not updated so we need to update it so it's block hash is in the database
                  spendingInfoDAO.updateAction(foundTxo)
              }
            })

        case None =>
          logger.debug(
            s"Skipping further processing of transaction=${transaction.txIdBE.hex}, already processed."
          )
          slick.dbio.DBIOAction.successful(foundTxo)
      }
    }
  }

  /** Adds utxos to the database that we are receiving */
  private def addReceivedUTXOs(
      outputsWithIndex: Seq[OutputWithIndex],
      transaction: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Write & Effect.Read] = {

    val spks = outputsWithIndex.map(_.output.scriptPubKey).toVector

    val addressDbWithOutputA = for {
      addressDbs <- addressDAO.findByScriptPubKeysAction(spks)
    } yield {
      if (addressDbs.isEmpty && outputsWithIndex.nonEmpty) {
        logger.warn(
          s"Found zero addresses in the database to match an output we have a script for, txid=${transaction.txIdBE.hex} outputs=$outputsWithIndex"
        )
      }
      matchAddressDbWithOutputs(addressDbs, outputsWithIndex.toVector)
    }

    val action = for {
      addressDbWithOutput <- addressDbWithOutputA
    } yield {
      val outputsVec = addressDbWithOutput.map { case (addressDb, out) =>
        require(addressDb.scriptPubKey == out.output.scriptPubKey)
        utxoHandling.processReceivedUtxoAction(
          transaction,
          out.index,
          blockHashWithConfsOpt,
          addressDb
        )
      }
      slick.dbio.DBIOAction.sequence(outputsVec)
    }

    action.flatten
  }

  /** Matches address dbs with outputs, drops addressDb/outputs that do not have
    * matches
    */
  private def matchAddressDbWithOutputs(
      addressDbs: Vector[AddressDb],
      outputsWithIndex: Vector[OutputWithIndex]
  ): Vector[(AddressDb, OutputWithIndex)] = {
    val addressDbsWithOutputsOpt = outputsWithIndex.map { out =>
      // find address associated with spk
      val addressDbOpt =
        addressDbs.find(_.scriptPubKey == out.output.scriptPubKey)
      addressDbOpt match {
        case None =>
          logger.warn(s"Could not find address associated with output=${out}")
          None
        case Some(addressDb) =>
          Some((addressDb, out))
      }
    }
    // get rid of outputs we couldn't match to an address
    val result = addressDbsWithOutputsOpt.flatten
    result
  }

  private[wallet] def insertIncomingTransaction(
      transaction: Transaction,
      incomingAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): DBIOAction[
    (TransactionDb, IncomingTransactionDb),
    NoStream,
    Effect.Read & Effect.Write
  ] = {
    val incomingDb = IncomingTransactionDb(transaction.txIdBE, incomingAmount)
    for {
      txDb <- insertTransactionAction(transaction, blockHashOpt)
      written <- incomingTxDAO.upsertAction(incomingDb)
    } yield (txDb, written)
  }

  /** Filters outputs on tx so that only relevant outputs to our wallet are
    * included
    */
  private def getRelevantOutputs(
      transaction: Transaction
  ): Future[Vector[OutputWithIndex]] = {
    val spks = transaction.outputs.map(_.scriptPubKey)
    scriptPubKeyDAO.findScriptPubKeys(spks.toVector).map { addrs =>
      matchReceivedTx(addrs, transaction)
    }
  }

  private def matchReceivedTx(
      addrs: Vector[ScriptPubKeyDb],
      transaction: Transaction
  ): Vector[OutputWithIndex] = {
    val withIndex =
      transaction.outputs.zipWithIndex
    withIndex.collect {
      case (out, idx) if addrs.map(_.scriptPubKey).contains(out.scriptPubKey) =>
        OutputWithIndex(out, idx)
    }.toVector
  }

  private def getRelevantOutputsForBlock(
      block: Block
  ): Future[Map[DoubleSha256DigestBE, Vector[OutputWithIndex]]] = {
    val spksInBlock: Vector[ScriptPubKey] = block.transactions
      .flatMap(tx => tx.outputs.map(o => o.scriptPubKey))
      .toVector
    val spksInDbF = scriptPubKeyDAO.findScriptPubKeys(spksInBlock)

    spksInDbF.map { addrs =>
      block.transactions.foldLeft(
        Map.empty[DoubleSha256DigestBE, Vector[OutputWithIndex]]
      ) { (acc, tx) =>
        acc.updated(tx.txIdBE, matchReceivedTx(addrs, tx))
      }
    }
  }

  /** Processes an incoming transaction that's new to us
    *
    * @return
    *   A list of inserted transaction outputs
    */
  private def processNewReceivedTx(
      transaction: Transaction,
      blockHashWithConfirmations: Option[BlockHashWithConfs],
      newTags: Vector[AddressTag],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Write & Effect.Read] = {
    if (relevantReceivedOutputs.isEmpty) {

      logger.trace(
        s"Found no outputs relevant to us in transaction${transaction.txIdBE.hex}"
      )

      DBIOAction.successful(Vector.empty)
    } else {
      val filteredOutputs =
        transaction.outputs.zipWithIndex.filter(o =>
          relevantReceivedOutputs.contains(OutputWithIndex(o._1, o._2)))

      if (filteredOutputs.isEmpty) {
        // no relevant outputs in this tx, return early
        DBIOAction.successful(Vector.empty)
      } else {
        val relevantReceivedOutputsForTx: Vector[OutputWithIndex] = {

          filteredOutputs.map { case (o, idx) =>
            OutputWithIndex(o, idx)
          }.toVector
        }

        val spks = relevantReceivedOutputsForTx.map(_.output.scriptPubKey)
        val spksInDbA = addressDAO.findByScriptPubKeysAction(spks)

        val ourOutputsA = for {
          spksInDb <- spksInDbA
        } yield {
          relevantReceivedOutputsForTx.collect {
            case OutputWithIndex(out, idx)
                if spksInDb.map(_.scriptPubKey).contains(out.scriptPubKey) =>
              OutputWithIndex(out, idx)
          }
        }

        val txDbA = for {
          ourOutputs <- ourOutputsA
          totalIncoming = ourOutputs.map(_.output.value).sum
          incomingTx <- insertIncomingTransaction(
            transaction,
            totalIncoming,
            blockHashWithConfirmations.map(_.blockHash)
          )
        } yield incomingTx

        val prevTagsDbA = for {
          (txDb, _) <- txDbA
          prevTagDbs <-
            addressTagDAO.findTxAction(txDb.transaction, networkParameters)
        } yield prevTagDbs

        val newTagsA = for {
          ourOutputs <- ourOutputsA
          prevTagDbs <- prevTagsDbA
          prevTags = prevTagDbs.map(_.addressTag)
          tagsToUse = prevTags
            .filterNot(tag => newTags.contains(tag)) ++ newTags
          newTagDbs = ourOutputs.flatMap { out =>
            val address = BitcoinAddress
              .fromScriptPubKey(out.output.scriptPubKey, networkParameters)
            tagsToUse.map(tag => AddressTagDb(address, tag))
          }
          created <- addressTagDAO.upsertAllAction(newTagDbs)
        } yield created

        val action = for {
          (txDb, _) <- txDbA
          ourOutputs <- ourOutputsA
          _ <- newTagsA
        } yield (ourOutputs, txDb)

        for {
          (ourOutputs, txDb) <- action
          utxos <- addReceivedUTXOs(outputsWithIndex = ourOutputs,
                                    transaction = txDb.transaction,
                                    blockHashWithConfsOpt =
                                      blockHashWithConfirmations)
        } yield utxos
      }
    }
  }
}
