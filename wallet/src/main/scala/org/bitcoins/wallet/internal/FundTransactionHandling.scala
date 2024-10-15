package org.bitcoins.wallet.internal

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.wallet.*
import org.bitcoins.core.api.wallet.db.{
  AccountDb,
  SpendingInfoDb,
  TransactionDb
}
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.wallet.builder.*
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.db.SafeDatabase
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{SpendingInfoDAO, TransactionDAO}
import org.bitcoins.wallet.WalletLogger
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}

import scala.concurrent.Future

case class FundTransactionHandling(
    accountHandling: AccountHandling,
    utxoHandling: UtxoHandling,
    addressHandling: AddressHandlingApi,
    transactionProcessing: TransactionProcessingApi,
    spendingInfoDAO: SpendingInfoDAO,
    transactionDAO: TransactionDAO,
    keyManager: BIP39KeyManagerApi,
    feeRateApi: FeeRateApi)(implicit
    walletConfig: WalletAppConfig,
    system: ActorSystem)
    extends FundTransactionHandlingApi
    with WalletLogger {
  // import walletConfig.profile.api._
  import org.bitcoins.core.currency.currencyUnitNumeric
  import system.dispatcher
  private val safeDatabase: SafeDatabase = spendingInfoDAO.safeDatabase

  override def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    for {
      account <- accountHandling.getDefaultAccount()
      funded <- fundRawTransaction(
        destinations = destinations,
        feeRate = feeRate,
        fromAccount = account,
        fromTagOpt = fromTagOpt,
        markAsReserved = markAsReserved
      )
    } yield funded
  }

  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      fromTagOpt: Option[AddressTag] = None,
      markAsReserved: Boolean = false
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    fundRawTransactionInternal(
      destinations = destinations,
      feeRate = feeRate,
      fromAccount = fromAccount,
      fromTagOpt = fromTagOpt,
      markAsReserved = markAsReserved
    )
  }

  /** Funds an unsigned transaction from the specified account */
  override def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      markAsReserved: Boolean
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    fundRawTransaction(
      destinations = destinations,
      feeRate = feeRate,
      fromAccount = fromAccount,
      fromTagOpt = None,
      markAsReserved = markAsReserved
    )
  }

  /** This returns a [[RawTxBuilder]] that can be used to generate an unsigned
    * transaction with [[RawTxBuilder.result()]] which can be signed with the
    * returned [[ScriptSignatureParams]].
    *
    * Utxos are funded with the given coin selection algorithm
    */
  private[bitcoins] def fundRawTransactionInternal(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      coinSelectionAlgo: CoinSelectionAlgo = CoinSelectionAlgo.LeastWaste,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    val action = fundRawTransactionInternalAction(
      destinations,
      feeRate,
      fromAccount,
      coinSelectionAlgo,
      fromTagOpt,
      markAsReserved
    )

    for {
      txHelper <- safeDatabase.run(action)
      _ <- txHelper.reservedUTXOsCallbackF
    } yield txHelper
  }

  private[bitcoins] def fundRawTransactionInternalAction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      coinSelectionAlgo: CoinSelectionAlgo = CoinSelectionAlgo.LeastWaste,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean
  ): DBIOAction[FundRawTxHelper[
                  ShufflingNonInteractiveFinalizer
                ],
                NoStream,
                Effect.Read & Effect.Write & Effect.Transactional] = {
    val amts = destinations.map(_.value)
    // need to allow 0 for OP_RETURN outputs
    require(
      amts.forall(_.satoshis.toBigInt >= 0),
      s"Cannot fund a transaction for a negative amount, got=$amts"
    )
    val amt = amts.sum
    logger.info(s"Attempting to fund a tx for amt=$amt with feeRate=$feeRate")
    val utxosA =
      for {
        utxos <- fromTagOpt match {
          case None =>
            spendingInfoDAO.findAllUnspentForAccountAction(
              fromAccount.hdAccount
            )
          case Some(tag) =>
            spendingInfoDAO.findAllUnspentForTagAction(tag).map { utxos =>
              utxos.filter(utxo =>
                HDAccount.isSameAccount(
                  bip32Path = utxo.privKeyPath,
                  account = fromAccount.hdAccount
                ))
            }
        }
        utxoWithTxs <- getPreviousTransactionsAction(utxos)

        // Need to remove immature coinbase inputs
        immatureCoinbases = utxoWithTxs.filter(
          _._1.state == TxoState.ImmatureCoinbase
        )
      } yield utxoWithTxs.filter(utxo =>
        !immatureCoinbases.exists(_._1 == utxo._1))

    val selectedUtxosA =
      for {
        walletUtxos <- utxosA

        // filter out dust
        selectableUtxos = walletUtxos
          .map(_._1)
          .filter(_.output.value > Policy.dustThreshold)
          .map(CoinSelectorUtxo.fromSpendingInfoDb)

        utxos = CoinSelector.selectByAlgo(
          coinSelectionAlgo = coinSelectionAlgo,
          walletUtxos = selectableUtxos,
          outputs = destinations,
          feeRate = feeRate,
          longTermFeeRateOpt = Some(walletConfig.longTermFeeRate)
        )
        filtered = walletUtxos.filter(utxo =>
          utxos.exists(_.outPoint == utxo._1.outPoint))
        (_, callbackF) <-
          if (markAsReserved)
            utxoHandling.markUTXOsAsReservedAction(filtered.map(_._1))
          else DBIO.successful((Vector.empty, Future.unit))
      } yield (filtered, callbackF)

    for {
      (selectedUtxos, callbackF) <- selectedUtxosA
      map = SpendingInfoDb.toPreviousOutputMap(selectedUtxos.map(_._1))
      change <- accountHandling.getNewChangeAddressAction(fromAccount)
      utxoSpendingInfos = {
        selectedUtxos.map { case (utxo, prevTx) =>
          utxo.toUTXOInfo(keyManager = keyManager,
                          prevTransaction = prevTx.transaction,
                          previousOutputMap = map)
        }
      }
    } yield {
      val utxosStr = selectedUtxos.map { utxo =>
        s"${utxo._1.outPoint} state=${utxo._1.state}"
      }
      logger.info(s"Spending UTXOs: $utxosStr")

      utxoSpendingInfos.zipWithIndex.foreach { case (utxo, index) =>
        logger.info(s"UTXO $index details: ${utxo.output}")
      }

      val txBuilder =
        ShufflingNonInteractiveFinalizer.txBuilderFrom(
          destinations,
          utxoSpendingInfos,
          feeRate,
          change.scriptPubKey
        )

      val fundTxHelper = FundRawTxHelper(
        txBuilderWithFinalizer = txBuilder,
        scriptSigParams = utxoSpendingInfos,
        feeRate = feeRate,
        reservedUTXOsCallbackF = callbackF
      )

      fundTxHelper
    }
  }

  def getPreviousTransactionsAction(utxos: Vector[SpendingInfoDb])
      : DBIOAction[Vector[(SpendingInfoDb, TransactionDb)],
                   NoStream,
                   Effect.Read] = {
    val nestedActions = utxos.map(u =>
      transactionDAO
        .findByTxIdAction(u.txid)
        .map(t =>
          (u, t.getOrElse(sys.error(s"Could find tx=${u.txid} for utxo=$u")))))
    DBIOAction.sequence(nestedActions)
  }

  def getPreviousTransactions(utxos: Vector[SpendingInfoDb])
      : Future[Vector[(SpendingInfoDb, TransactionDb)]] = {
    val action = getPreviousTransactionsAction(utxos)
    safeDatabase.run(action)
  }
}
