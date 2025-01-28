package com.bitcoins.clightning.rpc

import com.bitcoins.clightning.rpc.CLightningRpcClient.feeRateToJson
import com.bitcoins.clightning.rpc.config._
import org.bitcoins.commons.jsonmodels.clightning.CLightningJsonModels._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.util.{BitcoinSLogger, NativeProcessFactory}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.channel.{FundedChannelId, ShortChannelId}
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.{NodeId, NodeUri}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.core.wallet.fee._
import org.bitcoins.crypto.Sha256Digest
import play.api.libs.json._
import scodec.bits._

import java.io.File
import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}

class CLightningRpcClient(val instance: CLightningInstanceLocal, binary: File)(
    implicit val executionContext: ExecutionContext
) extends CLightningUnixSocketHandler
    with NativeProcessFactory
    with StartStopAsync[CLightningRpcClient]
    with BitcoinSLogger {

  // documentation: https://lightning.readthedocs.io/index.html

  def getInfo: Future[CLightningInfo] =
    clightningCall[CLightningInfo]("getinfo")

  def nodeId: Future[NodeId] = getInfo.map(_.id)

  def getNewAddress: Future[BitcoinAddress] =
    clightningCall[NewAddressResult]("newaddr").map(_.address)

  def getNewAddress(addressType: AddressType): Future[BitcoinAddress] = {
    val paramF = addressType match {
      case AddressType.SegWit => Future.successful(JsString("bech32"))
      case AddressType.P2TR =>
        Future.successful(JsString("p2tr"))
      case x @ (AddressType.Legacy | AddressType.NestedSegWit) =>
        Future.failed(
          new IllegalArgumentException(
            s"clightning cannot generate ${x.altName} addresses"
          )
        )
    }

    for {
      param <- paramF
      res <- clightningCall[NewAddressResult]("newaddr", JsArray(Vector(param)))
    } yield res.address

  }

  def listFunds: Future[ListFundsResult] = listFunds(false)

  def listFunds(spent: Boolean): Future[ListFundsResult] =
    clightningCall[ListFundsResult](
      "listfunds",
      JsArray(Vector(JsBoolean(spent)))
    )

  def walletBalance(): Future[WalletBalances] = {
    listFunds.map { funds =>
      val start = WalletBalances(Satoshis.zero, Satoshis.zero, Satoshis.zero)
      funds.outputs.foldLeft(start) { case (balances, utxo) =>
        val amt = utxo.amount_msat.toSatoshis
        val newTotal = balances.balance + amt
        utxo.status match {
          case OutputStatus.Spent => balances
          case OutputStatus.Unconfirmed =>
            val newUnconfirmed = balances.unconfirmedBalance + amt
            balances.copy(
              balance = newTotal,
              unconfirmedBalance = newUnconfirmed
            )
          case OutputStatus.Confirmed =>
            val newConfirmed = balances.confirmedBalance + amt
            balances.copy(balance = newTotal, confirmedBalance = newConfirmed)
        }
      }
    }
  }

  def connect(nodeId: NodeId, addr: InetSocketAddress): Future[ConnectResult] =
    connect(nodeId, addr.getHostString, addr.getPort)

  def connect(
      nodeId: NodeId,
      host: String,
      port: Int
  ): Future[ConnectResult] = {
    val uri = NodeUri(nodeId, host, port)
    connect(uri)
  }

  def connect(uri: NodeUri): Future[ConnectResult] = {
    val params = JsArray(
      Vector(
        JsString(uri.nodeId.toString),
        JsString(uri.host),
        JsNumber(uri.port)
      )
    )

    clightningCall[ConnectResult]("connect", params)
  }

  def connect(nodeId: NodeId): Future[ConnectResult] = {
    val params = JsArray(Vector(JsString(nodeId.toString)))
    clightningCall[ConnectResult]("connect", params)
  }

  def listPeers: Future[Vector[CLightningPeer]] = {
    clightningCall[CLightningPeers]("listpeers").map(_.peers)
  }

  def findPeer(nodeId: NodeId): Future[Option[CLightningPeer]] = {
    val params = JsArray(Vector(JsString(nodeId.toString)))
    clightningCall[CLightningPeers]("listpeers", params)
      .map(_.peers.headOption)
  }

  def isConnected(nodeId: NodeId): Future[Boolean] =
    findPeer(nodeId).map(_.exists(_.connected))

  def openChannel(
      nodeId: NodeId,
      fundingAmount: CurrencyUnit,
      pushAmt: CurrencyUnit,
      feeRate: FeeUnit,
      privateChannel: Boolean
  ): Future[FundChannelResult] = {

    val params = JsObject(
      Vector(
        "id" -> JsString(nodeId.toString),
        "amount" -> JsNumber(fundingAmount.satoshis.toLong),
        "feerate" -> feeRateToJson(feeRate),
        "announce" -> JsBoolean(privateChannel),
        "push_msat" -> JsNumber(MilliSatoshis(pushAmt).toLong)
      )
    )

    clightningCall[FundChannelResult]("fundchannel", params)
  }

  def closeChannel(id: NodeId): Future[CloseChannelResult] = {
    val params = JsArray(Vector(JsString(id.toString)))
    clightningCall[CloseChannelResult]("close", params)
  }

  def closeChannel(shortChanId: ShortChannelId): Future[CloseChannelResult] = {
    val params = JsArray(Vector(JsString(shortChanId.toString)))
    clightningCall[CloseChannelResult]("close", params)
  }

  def closeChannel(channelId: FundedChannelId): Future[CloseChannelResult] = {
    val params = JsArray(Vector(JsString(channelId.hex)))
    clightningCall[CloseChannelResult]("close", params)
  }

  def listChannels(): Future[Vector[Channel]] = {
    clightningCall[ListChannelsResult]("listchannels").map(_.channels)
  }

  def findChannel(shortChanId: ShortChannelId): Future[Option[Channel]] = {
    val params = JsArray(Vector(JsString(shortChanId.toString)))

    clightningCall[ListChannelsResult]("listchannels", params)
      .map(_.channels.headOption)
  }

  def createInvoice(
      amount: CurrencyUnit,
      label: String,
      description: String,
      expirySeconds: Long
  ): Future[CLightningInvoiceResult] = {
    val params = JsObject(
      Vector(
        "amount_msat" -> JsNumber(MilliSatoshis(amount).toLong),
        "label" -> JsString(label),
        "description" -> JsString(description),
        "expiry" -> JsNumber(expirySeconds)
      )
    )

    clightningCall[CLightningInvoiceResult]("invoice", params)
  }

  def payInvoice(invoice: LnInvoice): Future[CLightningPayResult] = {
    clightningCall[CLightningPayResult](
      "pay",
      JsArray(Vector(JsString(invoice.toString)))
    )
  }

  def payInvoice(
      invoice: LnInvoice,
      amount: CurrencyUnit
  ): Future[CLightningPayResult] = {
    val params = JsArray(
      Vector(JsString(invoice.toString), JsNumber(MilliSatoshis(amount).toLong))
    )

    clightningCall[CLightningPayResult]("pay", params)
  }

  def lookupInvoice(
      paymentHash: Sha256Digest
  ): Future[Option[CLightningLookupInvoiceResult]] = {
    val params = JsObject(Vector("payment_hash" -> JsString(paymentHash.hex)))
    clightningCall[CLightningListInvoicesResult]("listinvoices", params).map(
      _.invoices.headOption
    )
  }

  def listInvoices: Future[Vector[CLightningLookupInvoiceResult]] = {
    clightningCall[CLightningListInvoicesResult]("listinvoices").map(_.invoices)
  }

  def waitInvoice(label: String): Future[CLightningLookupInvoiceResult] = {
    clightningCall[CLightningLookupInvoiceResult](
      "waitinvoice",
      JsArray(Vector(JsString(label)))
    )
  }

  def reserveInputs(psbt: PSBT): Future[Vector[InputReservation]] = {
    val param = JsArray(Vector(JsString(psbt.base64)))
    clightningCall[InputReservations]("reserveinputs", param).map(
      _.reservations
    )
  }

  def reserveInputs(
      psbt: PSBT,
      exclusive: Boolean
  ): Future[Vector[InputReservation]] = {
    val param = JsArray(Vector(JsString(psbt.base64), JsBoolean(exclusive)))
    clightningCall[InputReservations]("reserveinputs", param).map(
      _.reservations
    )
  }

  def reserveInputs(
      psbt: PSBT,
      exclusive: Boolean,
      reserve: Int
  ): Future[Vector[InputReservation]] = {
    val param = JsArray(
      Vector(JsString(psbt.base64), JsBoolean(exclusive), JsNumber(reserve))
    )
    clightningCall[InputReservations]("reserveinputs", param).map(
      _.reservations
    )
  }

  def signPSBT(psbt: PSBT): Future[PSBT] = {
    clightningCall[CLightningPsbtResult](
      "signpsbt",
      JsArray(Vector(JsString(psbt.base64)))
    ).map(_.signed_psbt)
  }

  def signPSBT(psbt: PSBT, indexesToSign: Vector[Int]): Future[PSBT] = {
    val params = JsObject(
      Vector(
        "psbt" -> JsString(psbt.base64),
        "signonly" -> JsArray(indexesToSign.map(JsNumber(_)))
      )
    )

    clightningCall[CLightningPsbtResult]("signpsbt", params).map(_.signed_psbt)
  }

  def listTransactions(): Future[Vector[CLightningTransaction]] =
    clightningCall[ListTransactionsResults]("listtransactions").map(
      _.transactions
    )

  def withdraw(
      address: BitcoinAddress,
      amount: Satoshis
  ): Future[WithdrawResult] =
    sendToAddress(address, amount)

  def withdraw(
      address: BitcoinAddress,
      amount: Satoshis,
      feeRate: FeeUnit
  ): Future[WithdrawResult] =
    sendToAddress(address, amount, feeRate)

  def sendToAddress(
      address: BitcoinAddress,
      amount: Satoshis
  ): Future[WithdrawResult] = {
    val params = JsObject(
      Vector(
        "destination" -> JsString(address.toString),
        "satoshi" -> JsNumber(amount.toLong)
      )
    )

    clightningCall[WithdrawResult]("withdraw", params)
  }

  def sendToAddress(
      address: BitcoinAddress,
      amount: Satoshis,
      feeRate: FeeUnit
  ): Future[WithdrawResult] = {
    val params = JsObject(
      Vector(
        "destination" -> JsString(address.toString),
        "satoshi" -> JsNumber(amount.toLong),
        "feerate" -> feeRateToJson(feeRate)
      )
    )

    clightningCall[WithdrawResult]("withdraw", params)
  }

  def initChannelOpen(
      nodeId: NodeId,
      amount: CurrencyUnit,
      privateChannel: Boolean
  ): Future[FundChannelStartResult] = {
    val params = JsObject(
      Vector(
        "id" -> JsString(nodeId.toString),
        "amount" -> JsNumber(amount.satoshis.toLong),
        "announce" -> JsBoolean(privateChannel)
      )
    )

    clightningCall[FundChannelStartResult]("fundchannel_start", params)
  }

  def completeChannelOpen(
      nodeId: NodeId,
      psbt: PSBT
  ): Future[FundChannelCompleteResult] = {
    val params = JsArray(
      Vector(JsString(nodeId.toString), JsString(psbt.base64))
    )

    clightningCall[FundChannelCompleteResult]("fundchannel_complete", params)
  }

  def cancelChannelOpen(nodeId: NodeId): Future[FundChannelCancelResult] = {
    val params = JsArray(Vector(JsString(nodeId.toString)))

    clightningCall[FundChannelCancelResult]("fundchannel_cancel", params)
  }

  def sendCustomMessage(
      peer: NodeId,
      tpe: BigSizeUInt,
      data: ByteVector
  ): Future[SendCustomMessageResult] = {
    val tlv = TLV.fromTypeAndValue(tpe, data)
    sendCustomMessage(peer, tlv)
  }

  def sendCustomMessage(
      peer: NodeId,
      tlv: TLV
  ): Future[SendCustomMessageResult] = {
    val lnMessage = LnMessage[TLV](tlv)
    sendCustomMessage(peer, lnMessage)
  }

  def sendCustomMessage(
      peer: NodeId,
      lnMessage: LnMessage[TLV]
  ): Future[SendCustomMessageResult] = {
    val params = JsObject(
      Vector(
        "node_id" -> JsString(peer.toString),
        "msg" -> JsString(lnMessage.hex)
      )
    )

    clightningCall[SendCustomMessageResult]("sendcustommsg", params)
  }

  override val cmd: Vector[String] = {
    val logFileConf = instance.logFileOpt
      .map(f => s"--log-file=${f.getAbsolutePath}")
      .getOrElse("")

    Vector(
      binary.toString,
      s"--lightning-dir=${instance.datadir.toAbsolutePath}",
      s"--rpc-file=${instance.rpcFile.getAbsolutePath}",
      logFileConf
    )
  }

  override def start(): Future[CLightningRpcClient] = {
    startBinary().map(_ => this)
  }

  override def stop(): Future[CLightningRpcClient] = {
    clightningCall[String]("stop").map(_ => this)
  }
}

object CLightningRpcClient {

  /** The current version we support of clightning */
  val version = "24.11.1"

  private[clightning] def feeRateToJson(feeUnit: FeeUnit): JsString = {
    // clightning only takes SatoshisPerKiloByte or SatoshisPerKW
    // luckily all of our FeeUnits can be converted to one of these
    feeUnit match {
      case perKb: SatoshisPerKiloByte => JsString(s"${perKb.toLong}perkb")
      case perKW: SatoshisPerKW       => JsString(s"${perKW.toLong}perkw")
      case perByte: SatoshisPerByte   =>
        // convert to SatoshisPerKiloByte
        val perKb = perByte.toSatPerKb
        JsString(s"${perKb.toLong}perkb")
      case perVByte: SatoshisPerVirtualByte =>
        // convert to SatoshisPerKW
        val perKW = perVByte.toSatoshisPerKW
        JsString(s"${perKW.toLong}perkw")
    }
  }
}
