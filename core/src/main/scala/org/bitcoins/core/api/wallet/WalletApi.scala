package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.DoubleSha256DigestBE

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

/** API for the wallet project.
  *
  * This wallet API is BIP44 compliant.
  *
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait WalletApi {
  def accountHandling: AccountHandlingApi
  def fundTxHandling: FundTransactionHandlingApi
  def rescanHandling: RescanHandlingApi
  def addressHandling: AddressHandlingApi
  def utxoHandling: UtxoHandlingApi
  def transactionProcessing: TransactionProcessingApi
  def sendFundsHandling: SendFundsHandlingApi

  def nodeApi: NodeApi
  def chainQueryApi: ChainQueryApi
  def feeRateApi: FeeRateApi
  def creationTime: Instant

  def broadcastTransaction(transaction: Transaction): Future[Unit]

  def getFeeRate(): Future[FeeUnit] = feeRateApi.getFeeRate()

  /** Gets the sum of all UTXOs in this wallet */
  def getBalance()(implicit ec: ExecutionContext): Future[CurrencyUnit] = {
    val confirmedF = getConfirmedBalance()
    val unconfirmedF = getUnconfirmedBalance()

    for {
      confirmed <- confirmedF
      unconfirmed <- unconfirmedF
    } yield confirmed + unconfirmed
  }

  /** Gets the sum of all confirmed UTXOs in this wallet */
  def getConfirmedBalance(): Future[CurrencyUnit]

  def getNewAddress(): Future[BitcoinAddress]

  def getNewChangeAddress(): Future[BitcoinAddress]

  /** Gets the sum of all unconfirmed UTXOs in this wallet */
  def getUnconfirmedBalance(): Future[CurrencyUnit]

  /** Checks if the wallet contains any data */
  def isEmpty(): Future[Boolean]

  def getSyncState(): Future[BlockSyncState]

  def isRescanning(): Future[Boolean]

  def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]]

  def getWalletName(): Future[String]

  def getInfo(): Future[WalletInfo]
}

case class WalletInfo(
    walletName: String,
    rootXpub: ExtPublicKey,
    xpub: ExtPublicKey,
    hdAccount: HDAccount,
    height: Int,
    blockHash: DoubleSha256DigestBE,
    rescan: Boolean,
    imported: Boolean)

/** An HDWallet that uses Neutrino to sync */
trait NeutrinoHDWalletApi extends WalletApi with NeutrinoWalletApi
