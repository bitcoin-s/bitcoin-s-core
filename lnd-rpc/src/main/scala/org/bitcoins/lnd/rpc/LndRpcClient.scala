package org.bitcoins.lnd.rpc

import org.apache.pekko.NotUsed
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{Sink, Source}
import chainrpc._
import com.google.protobuf.ByteString
import invoicesrpc.LookupInvoiceMsg.InvoiceRef
import invoicesrpc._
import io.grpc.{CallCredentials, Metadata}
import lnrpc.ChannelPoint.FundingTxid.FundingTxidBytes
import lnrpc.CloseStatusUpdate.Update.{ChanClose, ClosePending}
import lnrpc._
import org.apache.pekko.grpc.{GrpcClientSettings, SSLContextUtils}
import org.bitcoins.commons.jsonmodels.lnd._
import org.bitcoins.commons.util.{BitcoinSLogger, NativeProcessFactory}
import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.LnTag.PaymentHashTag
import org.bitcoins.core.protocol.ln.channel.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput,
  Transaction => Tx
}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.core.wallet.fee.{SatoshisPerKW, SatoshisPerVirtualByte}
import org.bitcoins.crypto._
import org.bitcoins.lnd.rpc.LndRpcClient._
import org.bitcoins.lnd.rpc.LndUtils._
import org.bitcoins.lnd.rpc.config._
import org.bitcoins.lnd.rpc.internal._
import peersrpc.PeersClient
import routerrpc._
import scodec.bits._
import signrpc._
import verrpc._
import walletrpc.FundPsbtRequest.Fees.SatPerVbyte
import walletrpc.FundPsbtRequest.Template.Psbt
import walletrpc.{
  AddressType => _,
  ListUnspentRequest => _,
  Transaction => _,
  _
}

import java.io._
import java.net.InetSocketAddress
import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.Try

/** @param binaryOpt Path to lnd executable
  */
class LndRpcClient(val instance: LndInstance, binaryOpt: Option[File] = None)(
    implicit val system: ActorSystem)
    extends NativeProcessFactory
    with LndUtils
    with LndRouterClient
    with StartStopAsync[LndRpcClient]
    with BitcoinSLogger {
  instance match {
    case _: LndInstanceLocal =>
      require(binaryOpt.isDefined,
              s"Binary must be defined with a local instance of lnd")
    case _: LndInstanceRemote => ()
  }

  /** The command to start the daemon on the underlying OS */
  override def cmd: String = instance match {
    case local: LndInstanceLocal =>
      s"${binaryOpt.get} --lnddir=${local.datadir.toAbsolutePath}"
    case _: LndInstanceRemote => ""
  }

  implicit val executionContext: ExecutionContext = system.dispatcher

  // These need to be lazy so we don't try and fetch
  // the tls certificate before it is generated
  private[this] lazy val certStreamOpt: Option[InputStream] = {
    instance.certFileOpt match {
      case Some(file) => Some(new FileInputStream(file))
      case None =>
        instance.certificateOpt match {
          case Some(cert) =>
            Some(
              new ByteArrayInputStream(
                cert.getBytes(java.nio.charset.StandardCharsets.UTF_8.name)))
          case None => None
        }
    }
  }

  private lazy val callCredentials = new CallCredentials {

    def applyRequestMetadata(
        requestInfo: CallCredentials.RequestInfo,
        appExecutor: Executor,
        applier: CallCredentials.MetadataApplier
    ): Unit = {
      appExecutor.execute(() => {
        // Wrap in a try, in case the macaroon hasn't been created yet.
        Try {
          val metadata = new Metadata()
          val key =
            Metadata.Key.of(macaroonKey, Metadata.ASCII_STRING_MARSHALLER)
          metadata.put(key, instance.macaroon)
          applier(metadata)
        }
        ()
      })
    }

    def thisUsesUnstableApi(): Unit = ()
  }

  // Configure the client
  private lazy val clientSettings: GrpcClientSettings = {
    val trustManagerOpt = certStreamOpt match {
      case Some(stream) => Some(SSLContextUtils.trustManagerFromStream(stream))
      case None         => None
    }

    val client = GrpcClientSettings
      .connectToServiceAt(instance.rpcUri.getHost, instance.rpcUri.getPort)
      .withCallCredentials(callCredentials)

    trustManagerOpt match {
      case Some(trustManager) => client.withTrustManager(trustManager)
      case None               => client
    }
  }

  // Create a client-side stub for the services
  lazy val lnd: LightningClient = LightningClient(clientSettings)
  lazy val wallet: WalletKitClient = WalletKitClient(clientSettings)
  lazy val unlocker: WalletUnlockerClient = WalletUnlockerClient(clientSettings)
  lazy val signer: SignerClient = SignerClient(clientSettings)
  lazy val router: RouterClient = RouterClient(clientSettings)
  lazy val invoices: InvoicesClient = InvoicesClient(clientSettings)
  lazy val peersClient: PeersClient = PeersClient(clientSettings)
  lazy val stateClient: StateClient = StateClient(clientSettings)
  lazy val versionerClient: VersionerClient = VersionerClient(clientSettings)

  lazy val chainClient: ChainNotifierClient = ChainNotifierClient(
    clientSettings)

  def genSeed(): Future[GenSeedResponse] = {
    logger.trace("lnd calling genseed")

    val req = GenSeedRequest()
    unlocker
      .genSeed(req)
  }

  def initWallet(password: String): Future[ByteString] = {
    logger.trace("lnd calling initwallet")

    val passwordByteStr = ByteString.copyFromUtf8(password)

    for {
      seed <- genSeed()
      req = InitWalletRequest(walletPassword = passwordByteStr,
                              cipherSeedMnemonic = seed.cipherSeedMnemonic)
      res <- unlocker.initWallet(req).map(_.adminMacaroon)
    } yield res
  }

  def unlockWallet(password: String): Future[Unit] = {
    logger.trace("lnd calling unlockwallet")

    val byteStrPass = ByteString.copyFromUtf8(password)
    val req: UnlockWalletRequest =
      UnlockWalletRequest(walletPassword = byteStrPass)

    unlocker
      .unlockWallet(req)
      .map(_ => ())
  }

  def getInfo: Future[GetInfoResponse] = {
    logger.trace("lnd calling getinfo")

    val req = GetInfoRequest()

    lnd.getInfo(req)
  }

  def nodeId: Future[NodeId] = {
    getInfo.map(info => NodeId(info.identityPubkey))
  }

  def getVersion(): Future[Version] = {
    logger.trace("lnd calling getversion")
    versionerClient.getVersion(VersionRequest())
  }

  def lookupInvoice(rHash: PaymentHashTag): Future[Invoice] = {
    val hash = InvoiceRef.PaymentHash(rHash.bytes)
    val req = LookupInvoiceMsg(hash)

    lookupInvoice(req)
  }

  def lookupInvoice(req: LookupInvoiceMsg): Future[Invoice] = {
    logger.trace("lnd calling lookupinvoiceV2")

    invoices.lookupInvoiceV2(req)
  }

  def cancelInvoice(invoice: LnInvoice): Future[Unit] = {
    cancelInvoice(invoice.lnTags.paymentHash.hash)
  }

  def cancelInvoice(hash: Sha256Digest): Future[Unit] = {
    logger.trace("lnd calling cancelinvoice")

    invoices.cancelInvoice(CancelInvoiceMsg(hash.bytes)).map(_ => ())
  }

  def addInvoice(
      memo: String,
      value: Satoshis,
      expiry: Long): Future[AddInvoiceResult] = {
    val invoice: Invoice =
      Invoice(memo = memo, value = value.toLong, expiry = expiry)

    addInvoice(invoice)
  }

  def addInvoice(
      descriptionHash: Sha256Digest,
      value: Satoshis,
      expiry: Long): Future[AddInvoiceResult] = {
    val invoice: Invoice =
      Invoice(value = value.toLong,
              expiry = expiry,
              descriptionHash = descriptionHash.bytes)

    addInvoice(invoice)
  }

  def addInvoice(
      memo: String,
      value: MilliSatoshis,
      expiry: Long): Future[AddInvoiceResult] = {
    val invoice: Invoice =
      Invoice(memo = memo, valueMsat = value.toLong, expiry = expiry)

    addInvoice(invoice)
  }

  def addInvoice(
      descriptionHash: Sha256Digest,
      value: MilliSatoshis,
      expiry: Long): Future[AddInvoiceResult] = {
    val invoice: Invoice =
      Invoice(valueMsat = value.toLong,
              expiry = expiry,
              descriptionHash = descriptionHash.bytes)

    addInvoice(invoice)
  }

  def addInvoice(invoice: Invoice): Future[AddInvoiceResult] = {
    logger.trace("lnd calling addinvoice")

    lnd
      .addInvoice(invoice)
      .map { res =>
        AddInvoiceResult(
          PaymentHashTag(Sha256Digest(res.rHash)),
          LnInvoice.fromString(res.paymentRequest),
          res.addIndex,
          res.paymentAddr
        )
      }
  }

  def subscribeInvoices(): Source[Invoice, NotUsed] = {
    lnd.subscribeInvoices(InvoiceSubscription())
  }

  def subscribeTransactions(): Source[TxDetails, NotUsed] = {
    lnd
      .subscribeTransactions(GetTransactionsRequest())
      .map(LndTransactionToTxDetails)
  }

  def subscribeChannelEvents(): Source[ChannelEventUpdate, NotUsed] = {
    lnd.subscribeChannelEvents(ChannelEventSubscription())
  }

  def subscribePeerEvents(): Source[PeerEvent, NotUsed] = {
    lnd.subscribePeerEvents(PeerEventSubscription())
  }

  def subscribeChannelGraph(): Source[GraphTopologyUpdate, NotUsed] = {
    lnd.subscribeChannelGraph(GraphTopologySubscription())
  }

  def subscribeChannelBackups(): Source[ChanBackupSnapshot, NotUsed] = {
    lnd.subscribeChannelBackups(ChannelBackupSubscription())
  }

  def getNewAddress: Future[BitcoinAddress] = {
    logger.trace("lnd calling newaddress")

    val req: NewAddressRequest = NewAddressRequest(AddressType.TAPROOT_PUBKEY)

    lnd
      .newAddress(req)
      .map(r => BitcoinAddress.fromString(r.address))
  }

  def getNewAddress(addressType: AddressType): Future[BitcoinAddress] = {
    logger.trace("lnd calling newaddress")

    val req: NewAddressRequest = NewAddressRequest(addressType)

    lnd
      .newAddress(req)
      .map(r => BitcoinAddress.fromString(r.address))
  }

  def listUnspent: Future[Vector[UTXOResult]] = {
    val request = ListUnspentRequest(0, Int.MaxValue)
    listUnspent(request)
  }

  def listUnspent(request: ListUnspentRequest): Future[Vector[UTXOResult]] = {
    logger.trace("lnd calling listunspent")

    lnd
      .listUnspent(request)
      .map(_.utxos.toVector.map { utxo =>
        val outPointOpt = utxo.outpoint.map { out =>
          val txId = DoubleSha256DigestBE(out.txidStr)
          TransactionOutPoint(txId, out.outputIndex)
        }

        val spkBytes = ByteVector.fromValidHex(utxo.pkScript)

        UTXOResult(BitcoinAddress.fromString(utxo.address),
                   Satoshis(utxo.amountSat),
                   ScriptPubKey.fromAsmBytes(spkBytes),
                   outPointOpt,
                   utxo.confirmations)

      })
  }

  def connectPeer(nodeId: NodeId, addr: InetSocketAddress): Future[Unit] = {
    val lnAddr =
      LightningAddress(nodeId.hex, s"${addr.getHostName}:${addr.getPort}")

    val request: ConnectPeerRequest = ConnectPeerRequest(Some(lnAddr))

    connectPeer(request)
  }

  def connectPeer(
      nodeId: NodeId,
      addr: InetSocketAddress,
      permanent: Boolean): Future[Unit] = {
    val lnAddr: LightningAddress =
      LightningAddress(nodeId.hex, s"${addr.getHostName}:${addr.getPort}")

    val request: ConnectPeerRequest =
      ConnectPeerRequest(Some(lnAddr), permanent)

    connectPeer(request)
  }

  def connectPeer(request: ConnectPeerRequest): Future[Unit] = {
    logger.trace("lnd calling connectpeer")

    lnd
      .connectPeer(request)
      .map(_ => ())
  }

  def isConnected(nodeId: NodeId): Future[Boolean] = {
    listPeers().map { peers =>
      peers.exists(p => NodeId(p.pubKey) == nodeId)
    }
  }

  def listPeers(): Future[Vector[Peer]] = {
    logger.trace("lnd calling listpeers")

    val request: ListPeersRequest = ListPeersRequest()

    lnd
      .listPeers(request)
      .map(_.peers.toVector)
  }

  def openChannel(
      nodeId: NodeId,
      fundingAmount: CurrencyUnit,
      satPerVByte: SatoshisPerVirtualByte,
      privateChannel: Boolean): Future[Option[TransactionOutPoint]] = {
    val request = OpenChannelRequest(
      nodePubkey = nodeId.bytes,
      localFundingAmount = fundingAmount.satoshis.toLong,
      satPerVbyte = UInt64(satPerVByte.toLong),
      `private` = privateChannel
    )

    openChannel(request)
  }

  def openChannel(
      nodeId: NodeId,
      fundingAmount: CurrencyUnit,
      pushAmt: CurrencyUnit,
      satPerVByte: SatoshisPerVirtualByte,
      privateChannel: Boolean): Future[Option[TransactionOutPoint]] = {
    val request = OpenChannelRequest(
      nodePubkey = nodeId.bytes,
      localFundingAmount = fundingAmount.satoshis.toLong,
      pushSat = pushAmt.satoshis.toLong,
      satPerVbyte = UInt64(satPerVByte.toLong),
      `private` = privateChannel
    )

    openChannel(request)
  }

  def openChannel(
      request: OpenChannelRequest): Future[Option[TransactionOutPoint]] = {
    logger.trace("lnd calling openchannel")

    lnd
      .openChannelSync(request)
      .map { point =>
        point.fundingTxid.fundingTxidBytes match {
          case Some(bytes) =>
            val txId = DoubleSha256DigestBE(bytes)
            Some(TransactionOutPoint(txId, point.outputIndex))
          case None => None
        }
      }
  }

  def closeChannel(
      outPoint: TransactionOutPoint,
      force: Boolean,
      feeRate: SatoshisPerVirtualByte): Future[DoubleSha256DigestBE] = {
    val channelPoint =
      ChannelPoint(FundingTxidBytes(outPoint.txId.bytes), outPoint.vout)

    closeChannel(
      CloseChannelRequest(channelPoint = Some(channelPoint),
                          force = force,
                          satPerVbyte = UInt64(feeRate.toLong)))
  }

  def closeChannel(
      outPoint: TransactionOutPoint): Future[DoubleSha256DigestBE] = {
    val channelPoint =
      ChannelPoint(FundingTxidBytes(outPoint.txId.bytes), outPoint.vout)
    closeChannel(CloseChannelRequest(Some(channelPoint)))
  }

  def closeChannel(
      request: CloseChannelRequest): Future[DoubleSha256DigestBE] = {
    logger.trace("lnd calling closechannel")

    lnd
      .closeChannel(request)
      .map(_.update)
      .filter(t => t.isClosePending || t.isChanClose)
      .runWith(Sink.head)
      .collect {
        case ClosePending(closeUpdate) =>
          val txId = DoubleSha256Digest(closeUpdate.txid)
          txId.flip
        case ChanClose(chanClose) =>
          if (chanClose.success) {
            val txId = DoubleSha256Digest(chanClose.closingTxid)
            txId.flip
          } else {
            throw new RuntimeException(s"Channel close failed")
          }
      }
  }

  def abandonChannel(
      outPoint: TransactionOutPoint,
      pendingFundingShimOnly: Boolean): Future[Unit] = {
    val channelPoint: ChannelPoint = outPoint
    val request =
      AbandonChannelRequest(Some(channelPoint),
                            pendingFundingShimOnly = pendingFundingShimOnly,
                            iKnowWhatIAmDoing = true)

    abandonChannel(request)
  }

  def abandonChannel(request: AbandonChannelRequest): Future[Unit] = {
    logger.trace("lnd calling abandonChannel")

    lnd.abandonChannel(request).map(_ => ())
  }

  def listChannels(request: ListChannelsRequest =
    ListChannelsRequest()): Future[Vector[Channel]] = {
    logger.trace("lnd calling listchannels")

    lnd
      .listChannels(request)
      .map(_.channels.toVector)
  }

  def listPendingChannels(): Future[PendingChannelsResponse] = {
    logger.trace("lnd calling pendingchannels")

    lnd.pendingChannels(PendingChannelsRequest())
  }

  def findChannel(
      channelPoint: TransactionOutPoint): Future[Option[Channel]] = {
    listChannels().map { channels =>
      channels.find(
        _.channelPoint == s"${channelPoint.txId.hex}:${channelPoint.vout.toLong}")
    }
  }

  def findChannel(chanId: ShortChannelId): Future[Option[Channel]] = {
    listChannels().map { channels =>
      channels.find(_.chanId == chanId.u64)
    }
  }

  def walletBalance(): Future[WalletBalances] = {
    logger.trace("lnd calling walletbalance")

    lnd
      .walletBalance(WalletBalanceRequest())
      .map { bals =>
        WalletBalances(balance = Satoshis(bals.totalBalance),
                       unconfirmedBalance = Satoshis(bals.unconfirmedBalance),
                       confirmedBalance = Satoshis(bals.confirmedBalance))
      }
  }

  def channelBalance(): Future[ChannelBalances] = {
    logger.trace("lnd calling channelbalance")

    lnd
      .channelBalance(ChannelBalanceRequest())
      .map { bals =>
        ChannelBalances(
          localBalance =
            Satoshis(bals.localBalance.map(_.sat).getOrElse(UInt64.zero)),
          remoteBalance =
            Satoshis(bals.remoteBalance.map(_.sat).getOrElse(UInt64.zero)),
          unsettledLocalBalance = Satoshis(
            bals.unsettledLocalBalance.map(_.sat).getOrElse(UInt64.zero)),
          unsettledRemoteBalance = Satoshis(
            bals.unsettledRemoteBalance.map(_.sat).getOrElse(UInt64.zero)),
          pendingOpenLocalBalance = Satoshis(
            bals.pendingOpenLocalBalance.map(_.sat).getOrElse(UInt64.zero)),
          pendingOpenRemoteBalance = Satoshis(
            bals.pendingOpenRemoteBalance.map(_.sat).getOrElse(UInt64.zero))
        )
      }
  }

  def sendPayment(
      invoice: LnInvoice,
      timeout: FiniteDuration): Future[Payment] = {
    val request: SendPaymentRequest =
      SendPaymentRequest(paymentRequest = invoice.toString,
                         timeoutSeconds = timeout.toSeconds.toInt,
                         noInflightUpdates = true)

    sendPayment(request)
  }

  def sendPayment(
      invoice: LnInvoice,
      feeLimit: Satoshis,
      timeout: FiniteDuration): Future[Payment] = {
    val request: SendPaymentRequest =
      SendPaymentRequest(paymentRequest = invoice.toString,
                         timeoutSeconds = timeout.toSeconds.toInt,
                         feeLimitSat = feeLimit.toLong,
                         noInflightUpdates = true)

    sendPayment(request)
  }

  def sendPayment(
      nodeId: NodeId,
      amount: CurrencyUnit,
      timeout: FiniteDuration): Future[Payment] = {
    val request: SendPaymentRequest =
      SendPaymentRequest(dest = nodeId.bytes,
                         amt = amount.satoshis.toLong,
                         timeoutSeconds = timeout.toSeconds.toInt,
                         noInflightUpdates = true)

    sendPayment(request)
  }

  def sendPayment(request: SendPaymentRequest): Future[Payment] = {
    logger.trace("lnd calling sendpaymentV2")

    router
      .sendPaymentV2(request)
      .filter(!_.status.isInFlight)
      .runWith(Sink.head[Payment])
  }

  def sendOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: SatoshisPerVirtualByte,
      spendUnconfirmed: Boolean): Future[Tx] = {
    sendOutputs(outputs, feeRate.toSatoshisPerKW, spendUnconfirmed)
  }

  def sendOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: SatoshisPerKW,
      spendUnconfirmed: Boolean): Future[Tx] = {

    val request = SendOutputsRequest(satPerKw = feeRate.toLong,
                                     outputs = outputs,
                                     spendUnconfirmed = spendUnconfirmed)
    sendOutputs(request)
  }

  def sendOutputs(request: SendOutputsRequest): Future[Tx] = {
    logger.trace("lnd calling sendoutputs")

    wallet
      .sendOutputs(request)
      .map(res => Tx(res.rawTx))
  }

  def fundPSBT(
      inputs: Vector[TransactionOutPoint],
      outputs: Map[BitcoinAddress, CurrencyUnit],
      feeRate: SatoshisPerVirtualByte,
      spendUnconfirmed: Boolean): Future[PSBT] = {
    val outputMap = outputs.map { case (addr, amt) =>
      addr.toString -> amt.satoshis.toUInt64
    }
    val template = TxTemplate(inputs, outputMap)
    val rawTemplate = FundPsbtRequest.Template.Raw(template)
    val fees = SatPerVbyte(UInt64(feeRate.toLong))
    val request = FundPsbtRequest(template = rawTemplate,
                                  fees = fees,
                                  spendUnconfirmed = spendUnconfirmed)

    fundPSBT(request)
  }

  def fundPSBT(
      inputs: Vector[TransactionOutPoint],
      outputs: Map[BitcoinAddress, CurrencyUnit],
      feeRate: SatoshisPerVirtualByte,
      account: String,
      spendUnconfirmed: Boolean): Future[PSBT] = {
    val outputMap = outputs.map { case (addr, amt) =>
      addr.toString -> amt.satoshis.toUInt64
    }
    val template = TxTemplate(inputs, outputMap)
    val rawTemplate = FundPsbtRequest.Template.Raw(template)
    val fees = SatPerVbyte(UInt64(feeRate.toLong))
    val request = FundPsbtRequest(template = rawTemplate,
                                  fees = fees,
                                  account = account,
                                  spendUnconfirmed = spendUnconfirmed)

    fundPSBT(request)
  }

  def fundPSBT(
      psbt: PSBT,
      feeRate: SatoshisPerVirtualByte,
      account: String,
      spendUnconfirmed: Boolean): Future[PSBT] = {
    val template = Psbt(psbt.bytes)
    val fees = SatPerVbyte(UInt64(feeRate.toLong))
    val request = FundPsbtRequest(template = template,
                                  fees = fees,
                                  account = account,
                                  spendUnconfirmed = spendUnconfirmed)

    fundPSBT(request)
  }

  def fundPSBT(
      psbt: PSBT,
      feeRate: SatoshisPerVirtualByte,
      spendUnconfirmed: Boolean): Future[PSBT] = {
    val template = Psbt(psbt.bytes)
    val fees = SatPerVbyte(UInt64(feeRate.toLong))
    val request = FundPsbtRequest(template = template,
                                  fees = fees,
                                  spendUnconfirmed = spendUnconfirmed)

    fundPSBT(request)
  }

  def fundPSBT(psbt: PSBT, feeRate: SatoshisPerVirtualByte): Future[PSBT] = {
    val template = Psbt(psbt.bytes)
    val fees = SatPerVbyte(UInt64(feeRate.toLong))
    val request = FundPsbtRequest(template, fees)

    fundPSBT(request)
  }

  def fundPSBT(request: FundPsbtRequest): Future[PSBT] = {
    logger.trace("lnd calling fundpsbt")

    wallet
      .fundPsbt(request)
      .map(res => PSBT(res.fundedPsbt))
  }

  def signPSBT(psbt: PSBT): Future[PSBT] = {
    val request = SignPsbtRequest(psbt.bytes)
    signPSBT(request)
  }

  def signPSBT(request: SignPsbtRequest): Future[PSBT] = {
    logger.trace("lnd calling signpsbt")

    wallet
      .signPsbt(request)
      .map(res => PSBT(res.signedPsbt))
  }

  def finalizePSBT(psbt: PSBT): Future[PSBT] = {
    val request = FinalizePsbtRequest(psbt.bytes)

    finalizePSBT(request)
  }

  def finalizePSBT(request: FinalizePsbtRequest): Future[PSBT] = {
    logger.trace("lnd calling finalizepsbt")

    wallet
      .finalizePsbt(request)
      .map(res => PSBT(res.signedPsbt))
  }

  def computeInputScript(
      tx: Tx,
      inputIdx: Int,
      hashType: HashType,
      output: TransactionOutput,
      signMethod: SignMethod,
      prevOuts: Vector[TransactionOutput]): Future[
    (ScriptSignature, ScriptWitness)] = {
    val signDescriptor =
      SignDescriptor(output = Some(output),
                     sighash = UInt32(hashType.num),
                     inputIndex = inputIdx,
                     signMethod = signMethod)

    val request: SignReq =
      SignReq(tx.bytes, Vector(signDescriptor), prevOuts)

    computeInputScript(request).map(_.head)
  }

  def computeInputScript(
      tx: Tx,
      inputIdx: Int,
      output: TransactionOutput,
      signMethod: SignMethod): Future[(ScriptSignature, ScriptWitness)] = {
    val signDescriptor =
      SignDescriptor(output = Some(output),
                     sighash = UInt32(HashType.sigHashAll.num),
                     inputIndex = inputIdx,
                     signMethod = signMethod)

    computeInputScript(tx, Vector(signDescriptor)).map(_.head)
  }

  def computeInputScript(
      tx: Tx,
      inputIdx: Int,
      output: TransactionOutput): Future[(ScriptSignature, ScriptWitness)] = {
    val signDescriptor =
      SignDescriptor(output = Some(output),
                     sighash = UInt32(HashType.sigHashAll.num),
                     inputIndex = inputIdx)

    computeInputScript(tx, Vector(signDescriptor)).map(_.head)
  }

  def computeInputScript(
      tx: Tx,
      signDescriptors: Vector[SignDescriptor]): Future[
    Vector[(ScriptSignature, ScriptWitness)]] = {
    val request: SignReq =
      SignReq(tx.bytes, signDescriptors)

    computeInputScript(request)
  }

  def computeInputScript(
      request: SignReq): Future[Vector[(ScriptSignature, ScriptWitness)]] = {
    logger.trace("lnd calling computeinputscript")

    signer.computeInputScript(request).map { res =>
      res.inputScripts.map { script =>
        val scriptSig = ScriptSignature.fromAsmBytes(script.sigScript)
        val witness = ScriptWitness(script.witness.reverse.toVector)

        (scriptSig, witness)
      }.toVector
    }
  }

  def listLeases(): Future[Vector[UTXOLease]] = {
    listLeases(ListLeasesRequest())
  }

  def listLeases(request: ListLeasesRequest): Future[Vector[UTXOLease]] = {
    logger.trace("lnd calling listleases")

    wallet
      .listLeases(request)
      .map(_.lockedUtxos.toVector.map { lease =>
        val txId = DoubleSha256DigestBE(lease.outpoint.get.txidBytes)
        val vout = lease.outpoint.get.outputIndex
        val outPoint = TransactionOutPoint(txId, vout)
        UTXOLease(lease.id, outPoint, lease.expiration.toLong)
      })
  }

  def leaseOutput(
      outpoint: TransactionOutPoint,
      leaseSeconds: Long): Future[UInt64] = {
    val outPoint =
      OutPoint(outpoint.txId.bytes, outputIndex = outpoint.vout)

    val request = LeaseOutputRequest(id = LndRpcClient.leaseId,
                                     outpoint = Some(outPoint),
                                     expirationSeconds = UInt64(leaseSeconds))

    leaseOutput(request)
  }

  /** LeaseOutput locks an output to the given ID, preventing it from being available for any future coin selection attempts.
    * The absolute time of the lock's expiration is returned.
    * The expiration of the lock can be extended by successive invocations of this RPC.
    * @param request LeaseOutputRequest
    * @return Unix timestamp for when the lease expires
    */
  def leaseOutput(request: LeaseOutputRequest): Future[UInt64] = {
    logger.trace("lnd calling leaseoutput")

    wallet.leaseOutput(request).map(_.expiration)
  }

  def releaseOutput(outpoint: TransactionOutPoint): Future[Unit] = {
    val outPoint =
      OutPoint(outpoint.txId.bytes, outputIndex = outpoint.vout)

    val request =
      ReleaseOutputRequest(id = LndRpcClient.leaseId, outpoint = Some(outPoint))

    releaseOutput(request)
  }

  def releaseOutput(request: ReleaseOutputRequest): Future[Unit] = {
    logger.trace("lnd calling releaseoutput")

    wallet.releaseOutput(request).map(_ => ())
  }

  def sendCustomMessage(
      peer: NodeId,
      lnMessage: LnMessage[TLV]): Future[Unit] = {
    sendCustomMessage(peer, lnMessage.tlv)
  }

  def sendCustomMessage(peer: NodeId, tlv: TLV): Future[Unit] = {
    sendCustomMessage(peer, tlv.tpe, tlv.value)
  }

  def sendCustomMessage(
      peer: NodeId,
      tpe: BigSizeUInt,
      data: ByteVector): Future[Unit] = {
    val request =
      SendCustomMessageRequest(peer = peer.bytes,
                               `type` = UInt32(tpe.toBigInt),
                               data = data)
    sendCustomMessage(request)
  }

  def sendCustomMessage(request: SendCustomMessageRequest): Future[Unit] = {
    logger.trace("lnd calling sendcustommessage")

    lnd.sendCustomMessage(request).map(_ => ())
  }

  def subscribeCustomMessages(): Source[(NodeId, TLV), NotUsed] = {
    lnd.subscribeCustomMessages(SubscribeCustomMessagesRequest()).map {
      response =>
        val nodeId = NodeId(response.peer)
        val tpe = BigSizeUInt(response.`type`.toBigInt)
        val tlv = TLV.fromTypeAndValue(tpe, response.data)

        (nodeId, tlv)
    }
  }

  /** Broadcasts the given transaction
    * @return None if no error, otherwise the error string
    */
  def publishTransaction(tx: Tx): Future[Option[String]] = {
    logger.trace("lnd calling publishtransaction")

    val request = walletrpc.Transaction(tx.bytes)

    wallet
      .publishTransaction(request)
      .map { res =>
        if (res.publishError.isEmpty) None
        else Some(res.publishError)
      }
  }

  def getTransaction(txId: DoubleSha256DigestBE): Future[Option[TxDetails]] = {
    // Idk why they don't have a separate function to just get one tx
    getTransactions().map(_.find(_.txId == txId))
  }

  def getTransactions(): Future[Vector[TxDetails]] = {
    getTransactions(startHeight = 0)
  }

  def getTransactions(startHeight: Int): Future[Vector[TxDetails]] = {
    getTransactions(startHeight, -1)
  }

  def getTransactions(
      startHeight: Int,
      endHeight: Int): Future[Vector[TxDetails]] = {
    getTransactions(GetTransactionsRequest(startHeight, endHeight))
  }

  def getTransactions(
      request: GetTransactionsRequest): Future[Vector[TxDetails]] = {
    logger.trace("lnd calling gettransactions")

    lnd
      .getTransactions(request)
      .map(_.transactions.toVector.map(LndTransactionToTxDetails))
  }

  def subscribeTxConfirmation(
      script: ScriptPubKey,
      requiredConfs: Int,
      heightHint: Int): Future[ConfDetails] = {
    require(heightHint > 0,
            s"heightHint must be greater than 0, got $heightHint")

    val request =
      ConfRequest(txid = DoubleSha256Digest.empty.bytes,
                  script = script.asmBytes,
                  numConfs = UInt32(requiredConfs),
                  heightHint = UInt32(heightHint))

    registerConfirmationsNotification(request)
      .filter(_.event.isConf)
      .runWith(Sink.head)
      .map(_.getConf)
  }

  def subscribeTxConfirmation(
      txId: DoubleSha256Digest,
      script: ScriptPubKey,
      requiredConfs: Int,
      heightHint: Int): Future[ConfDetails] = {
    require(heightHint > 0,
            s"heightHint must be greater than 0, got $heightHint")

    val request =
      ConfRequest(txid = txId.bytes,
                  script = script.asmBytes,
                  numConfs = UInt32(requiredConfs),
                  heightHint = UInt32(heightHint))

    registerConfirmationsNotification(request)
      .filter(_.event.isConf)
      .runWith(Sink.head)
      .map(_.getConf)
  }

  def registerConfirmationsNotification(
      request: ConfRequest): Source[ConfEvent, NotUsed] = {
    logger.trace("lnd calling RegisterConfirmationsNtfn")

    chainClient.registerConfirmationsNtfn(request)
  }

  def monitorInvoice(
      rHash: PaymentHashTag,
      interval: FiniteDuration = 1.second,
      maxAttempts: Int = 60): Future[Invoice] = {
    val p: Promise[Invoice] = Promise[Invoice]()
    val attempts = new AtomicInteger(0)
    val runnable = new Runnable() {

      def run(): Unit = {
        val receivedInfoF = lookupInvoice(rHash)

        //register callback that publishes a payment to our actor system's
        //event stream,
        receivedInfoF.foreach { case info: Invoice =>
          if (info.state.isSettled) {
            //invoice has been paid, let's publish to event stream
            //so subscribers so the even stream can see that a payment
            //was received
            //we need to create a `PaymentSucceeded`
            system.eventStream.publish(info)

            //complete the promise so the runnable will be canceled
            p.success(info)
          } else if (attempts.incrementAndGet() >= maxAttempts) {
            // too many tries to get info about a payment
            // either Lnd is down or the payment is still in PENDING state for some reason
            // complete the promise with an exception so the runnable will be canceled
            p.failure(
              new RuntimeException(
                s"LndApi.monitorInvoice() [$instance] too many attempts: ${attempts
                  .get()} for invoice=${rHash.hash.hex}"))
          }
        }
      }
    }

    val cancellable =
      system.scheduler.scheduleAtFixedRate(interval, interval)(runnable)

    p.future.onComplete(_ => cancellable.cancel())

    p.future
  }

  /** Starts lnd on the local system.
    */
  override def start(): Future[LndRpcClient] = {
    startBinary().map(_ => this)
  }

  /** Boolean check to verify the state of the client
    * @return Future Boolean representing if client has started
    */
  def isStarted: Future[Boolean] = {
    val p = Promise[Boolean]()

    val t = Try {
      val getStateF = stateClient.getState(GetStateRequest())
      val state = Await.result(getStateF, 5.seconds)

      state.state match {
        case WalletState.SERVER_ACTIVE =>
          p.trySuccess(true)
        case _: WalletState.Unrecognized | WalletState.WAITING_TO_START |
            WalletState.UNLOCKED | WalletState.LOCKED |
            WalletState.NON_EXISTING | WalletState.RPC_ACTIVE =>
          p.trySuccess(false)
      }
    }

    t.failed.foreach { _ =>
      p.trySuccess(false)
    }

    p.future
  }

  /** Returns a Future LndRpcClient if able to shut down
    * Lnd instance, inherits from the StartStop trait
    * @return A future LndRpcClient that is stopped
    */
  override def stop(): Future[LndRpcClient] = {
    logger.trace("lnd calling stop daemon")

    val stopF =
      lnd
        .stopDaemon(StopRequest())
        .flatMap(_ => lnd.close())

    for {
      _ <- stopF
      _ <- stopBinary()
      _ <- {
        if (system.name == LndRpcClient.ActorSystemName)
          system.terminate()
        else Future.unit
      }
    } yield this
  }

  /** Checks to see if the client stopped successfully
    * @return
    */
  def isStopped: Future[Boolean] = {
    isStarted.map(started => !started)
  }
}

object LndRpcClient {

  /** Lease id should be unique per application
    * this is the sha256 of "lnd bitcoin-s"
    */
  val leaseId: ByteString =
    hex"8c45ee0b90e3afd0fb4d6f39afa3c5d551ee5f2c7ac2d06820ed3d16582186d2"

  /** The current version we support of Lnd */
  private[bitcoins] val version = "v0.17.3-beta"

  /** Key used for adding the macaroon to the gRPC header */
  private[lnd] val macaroonKey = "macaroon"

  /** THe name we use to create actor systems. We use this to know which
    * actor systems to shut down on node shutdown
    */
  private[lnd] val ActorSystemName = "lnd-rpc-client-created-by-bitcoin-s"

  /** Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, where you need fine grained control
    * over the RPC client.
    */
  def apply(
      instance: LndInstance,
      binary: Option[File] = None): LndRpcClient = {
    implicit val system: ActorSystem = ActorSystem.create(ActorSystemName)
    withActorSystem(instance, binary)
  }

  /** Constructs a RPC client from the given datadir, or
    * the default datadir if no directory is provided
    */
  def withActorSystem(instance: LndInstance, binary: Option[File] = None)(
      implicit system: ActorSystem) = new LndRpcClient(instance, binary)
}
