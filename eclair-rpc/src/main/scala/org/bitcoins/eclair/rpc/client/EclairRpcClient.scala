package org.bitcoins.eclair.rpc.client

import java.io.File
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.crypto.Sha256Digest
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.{Address, NetworkElement}
import org.bitcoins.core.protocol.ln.channel.{ChannelId, FundedChannelId}
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnit, MilliSatoshis}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln.{LnInvoice, LnParams, PaymentPreimage, ShortChannelId}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.eclair.rpc.api.EclairApi
import org.bitcoins.eclair.rpc.config.EclairInstance
import org.bitcoins.eclair.rpc.json._
import org.bitcoins.eclair.rpc.network.{NodeUri, PeerState}
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.util.AsyncUtil
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.sys.process._
import scala.util.{Failure, Properties, Success}

class EclairRpcClient(val instance: EclairInstance)(
    implicit system: ActorSystem)
    extends EclairApi {
  import JsonReaders._

  private val resultKey = "result"
  private val errorKey = "error"
  implicit val m = ActorMaterializer.create(system)
  implicit val ec: ExecutionContext = m.executionContext
  private val logger = LoggerFactory.getLogger(this.getClass)

  def getDaemon: EclairInstance = instance

  override def allChannels(): Future[Vector[ChannelDesc]] = {
    eclairCallNew[Vector[ChannelDesc]]("allchannels")
  }

  override def allNodes(): Future[Vector[NodeInfo]] = {
    eclairCallNew[Vector[NodeInfo]]("allnodes")
  }

  override def allUpdates(): Future[Vector[ChannelUpdate]] =
    eclairCallNew[Vector[ChannelUpdate]]("allupdates")

  override def allUpdates(nodeId: NodeId): Future[Vector[ChannelUpdate]] =
    eclairCallNew[Vector[ChannelUpdate]]("allupdates", "nodeId" -> nodeId.toString)

  /**
    * @inheritdoc
    */
  override def audit(): Future[AuditResult] =
    eclairCallNew[AuditResult]("audit")

  /**
    * @inheritdoc
    */
  override def audit(from: Long, to: Long): Future[AuditResult] =
    eclairCallNew[AuditResult]("audit", "from" -> from.toString, "to" -> to.toString)

  override def channel(channelId: ChannelId): Future[ChannelResult] = {
    eclairCallNew[ChannelResult]("channel", "channelId" -> channelId.hex)
  }

  private def channels(nodeId: Option[NodeId]): Future[Vector[ChannelInfo]] = {
    val params = Seq().flatMap(_ => nodeId.map(id => "nodeId" -> id.toString))
    eclairCallNew[Vector[ChannelInfo]]("channels", params: _*)
  }

  def channels(): Future[Vector[ChannelInfo]] = channels(nodeId = None)

  override def channels(nodeId: NodeId): Future[Vector[ChannelInfo]] =
    channels(Option(nodeId))

  override def checkInvoice(invoice: LnInvoice): Future[PaymentRequest] = {
    eclairCall[PaymentRequest]("checkinvoice", List(JsString(invoice.toString)))
  }

  override def checkPayment(
      invoiceOrHash: Either[LnInvoice, Sha256Digest]): Future[Boolean] = {

    val string = {
      if (invoiceOrHash.isLeft) {
        invoiceOrHash.left.get.toString
      } else {
        invoiceOrHash.right.get.hex
      }
    }

    eclairCall[Boolean]("checkpayment", List(JsString(string)))
  }

  private def close(
      channelId: ChannelId,
      scriptPubKey: Option[ScriptPubKey]): Future[String] = {
    val params =
      if (scriptPubKey.isEmpty) {
        Seq("channelId" -> channelId.hex)
      } else {

        val asmHex = BitcoinSUtil.encodeHex(scriptPubKey.get.asmBytes)

        Seq("channelId" -> channelId.hex, "scriptPubKey" -> asmHex)
      }

    eclairCallNew[String]("close", params: _*)
  }

  def close(channelId: ChannelId): Future[String] =
    close(channelId, scriptPubKey = None)

  override def close(
      channelId: ChannelId,
      scriptPubKey: ScriptPubKey): Future[String] = {
    close(channelId, Some(scriptPubKey))
  }

  override def connect(nodeId: NodeId, host: String, port: Int): Future[String] = {
    val uri = NodeUri(nodeId, host, port)
    connect(uri)
  }

  override def connect(uri: NodeUri): Future[String] = {
    eclairCallNew[String]("connect", "uri" -> uri.toString)
  }

  override def findRoute(nodeId: NodeId, amountMsat: MilliSatoshis): Future[Vector[NodeId]] = {
    eclairCallNew[Vector[NodeId]]("findroutetonode", "nodeId" -> nodeId.toString, "amountMsat" -> amountMsat.toBigDecimal.toString)
  }

  override def findRoute(invoice: LnInvoice, amountMsat: Option[MilliSatoshis]): Future[Vector[NodeId]] = {
    val params = Seq(Some("invoice" -> invoice.toString), amountMsat.map(x => "amountMsat" -> x.toBigDecimal.toString)).flatten
    eclairCallNew[Vector[NodeId]]("findroute", params: _*)
  }

  override def forceClose(channelId: ChannelId): Future[String] = {
    eclairCallNew[String]("forceclose", "channelId" -> channelId.hex)
  }

  override def forceClose(shortChannelId: ShortChannelId): Future[String] = {
    eclairCallNew[String]("forceclose", "shortChannelId" -> shortChannelId.toString)
  }

  override def getInfo: Future[GetInfoResult] = {
    val result = eclairCallNew[GetInfoResult]("getinfo")
    result
  }

  override def getNodeURI: Future[NodeUri] = {
    getInfo.map { info =>
      val id = info.nodeId
      val host = instance.uri.getHost
      val port = instance.uri.getPort
      NodeUri(nodeId = id, host = host, port = port)
    }
  }

  def help: Future[Vector[String]] = {
    eclairCallNew[Vector[String]]("help")
  }

  override def isConnected(nodeId: NodeId): Future[Boolean] = {
    getPeers.map(
      _.exists(p => p.nodeId == nodeId && p.state == PeerState.CONNECTED))
  }

  override def network: LnParams = {
    LnParams.fromNetworkParameters(instance.network)
  }

  override def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      pushMsat: Option[MilliSatoshis],
      feerateSatPerByte: Option[SatoshisPerByte],
      channelFlags: Option[Byte]): Future[FundedChannelId] = {
    val _pushMsat = pushMsat.getOrElse(MilliSatoshis.zero).toBigDecimal.toString
    val _fundingSatoshis = fundingSatoshis.satoshis.toBigDecimal.toString

    val params: Seq[(String, String)] = {
      if (feerateSatPerByte.isEmpty) {
        Seq("nodeId" -> nodeId.toString,
            "fundingSatoshis" -> _fundingSatoshis,
            "_pushMsat" -> _pushMsat)
      } else if (channelFlags.isEmpty) {
        Seq("nodeId" -> nodeId.toString,
            "fundingSatoshis" -> _fundingSatoshis,
            "_pushMsat" -> _pushMsat,
            "fundingFeerateSatByte" -> feerateSatPerByte.get.toLong.toString)
      } else {
        Seq("nodeId" -> nodeId.toString,
          "fundingSatoshis" -> _fundingSatoshis,
          "_pushMsat" -> _pushMsat,
          "fundingFeerateSatByte" -> feerateSatPerByte.get.toLong.toString,
          "channelFlags" -> channelFlags.get.toString)
      }
    }

    //this is unfortunately returned in this format
    //created channel 30bdf849eb9f72c9b41a09e38a6d83138c2edf332cb116dd7cf0f0dfb66be395
    val call = eclairCallNew[String]("open", params: _*)

    //let's just return the chanId
    val chanIdF = call.map(_.split(" ").last)

    chanIdF.map(FundedChannelId.fromHex)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         pushMsat = None,
         feerateSatPerByte = None,
         channelFlags = None)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      pushMsat: MilliSatoshis): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         Some(pushMsat),
         feerateSatPerByte = None,
         channelFlags = None)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      pushMsat: MilliSatoshis,
      feerateSatPerByte: SatoshisPerByte): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         Some(pushMsat),
         Some(feerateSatPerByte),
         channelFlags = None)
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      pushMsat: MilliSatoshis = MilliSatoshis.zero,
      feerateSatPerByte: SatoshisPerByte,
      channelFlags: Byte): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         Some(pushMsat),
         Some(feerateSatPerByte),
         Some(channelFlags))
  }

  def open(
      nodeId: NodeId,
      fundingSatoshis: CurrencyUnit,
      feerateSatPerByte: SatoshisPerByte,
      channelFlags: Byte): Future[FundedChannelId] = {
    open(nodeId,
         fundingSatoshis,
         pushMsat = None,
         Some(feerateSatPerByte),
         Some(channelFlags))
  }

  override def getPeers: Future[Vector[PeerInfo]] = {
    eclairCallNew[Vector[PeerInfo]]("peers")
  }


  override def createInvoice(description: String, amountMsat: Option[LnCurrencyUnit], expireIn: Option[Long], fallbackAddress: Option[Address], paymentPreimage: Option[PaymentPreimage]): Future[LnInvoice] = {
    val params = Seq(
      Some("description" -> description),
      amountMsat.map(x => "amountMsat" -> x.toMSat.toBigDecimal.toString),
      expireIn.map(x => "expireIn" -> x.toString),
      fallbackAddress.map(x => "fallbackAddress" -> x.toString),
      paymentPreimage.map(x => "paymentPreimage" -> x.hex)
    ).flatten

    val responseF = eclairCallNew[CreateInvoiceResult]("createinvoice", params: _*)

    responseF.flatMap {
      res =>
        Future.fromTry(LnInvoice.fromString(res.serialized))
    }
  }

  override def parseInvoice(invoice: LnInvoice): Future[Unit] = {
    eclairCallNew[CreateInvoiceResult]("parseinvoice", "invoice" -> invoice.toString).map(_ => ())
  }

  override def payInvoice(invoice: LnInvoice, amountMsat: Option[LnCurrencyUnit], maxAttempts: Option[Int], feeThresholdSat: Option[LnCurrencyUnit], maxFeePct: Option[Int]): Future[String] = {
    val params = Seq(
      Some("invoice" -> invoice.toString),
      amountMsat.map(x => "amountMsat" -> x.toMSat.toBigDecimal.toString),
      maxAttempts.map(x => "maxAttempts" -> x.toString),
      feeThresholdSat.map(x => "feeThresholdSat" -> x.toSatoshis.toBigDecimal.toString),
      maxFeePct.map(x => "maxFeePct" -> x.toString)
    ).flatten

    eclairCallNew[String]("payinvoice", params: _*)
  }

  override def getReceivedInfo(paymentHash: Sha256Digest): Future[ReceivedPaymentResult] = {
    eclairCallNew[ReceivedPaymentResult]("getreceivedinfo", "paymentHash" -> paymentHash.hex)
  }

  override def getReceivedInfo(invoice: LnInvoice): Future[ReceivedPaymentResult] = {
    eclairCallNew[ReceivedPaymentResult]("getreceivedinfo", "invoice" -> invoice.toString)
  }

  override def getSentInfo(paymentHash: Sha256Digest): Future[Vector[PaymentResult]] = {
    eclairCallNew[Vector[PaymentResult]]("getsentinfo", "paymentHash" -> paymentHash.hex)
  }

  override def getSentInfo(id: String): Future[Vector[PaymentResult]] = {
    eclairCallNew[Vector[PaymentResult]]("getsentinfo", "id" -> id)
  }


  /** A way to generate a [[org.bitcoins.core.protocol.ln.LnInvoice LnInvoice]]
    * with eclair.
    *
    * @param amountMsat the amount to be encoded in the invoice
    * @param description meta information about the invoice
    * @param expirySeconds when the invoice expires
    * @return
    */
  override def receive(
      amountMsat: Option[LnCurrencyUnit],
      description: Option[String],
      expirySeconds: Option[Long]): Future[LnInvoice] = {
    val msat = amountMsat.map(_.toMSat)

    val params = {
      if (amountMsat.isEmpty) {
        List(JsString(description.getOrElse("")))
      } else {
        val amt = JsNumber(msat.get.toBigDecimal)
        if (expirySeconds.isEmpty) {
          List(amt, JsString(description.getOrElse("")))
        } else {
          List(amt,
               JsString(description.getOrElse("")),
               JsNumber(expirySeconds.get))
        }
      }
    }

    val serializedF = eclairCall[String]("receive", params)

    serializedF.flatMap { str =>
      val invoiceTry = LnInvoice.fromString(str)
      invoiceTry match {
        case Success(i) =>
          //register a monitor for when the payment is received
          registerPaymentMonitor(i)

          Future.successful(i)
        case Failure(err) =>
          Future.failed(err)
      }
    }
  }

  /**
    * Pings eclair every second to see if a invoice has been paid
    * If the invoice has bene paid, we publish a
    * [[org.bitcoins.eclair.rpc.json.PaymentSucceeded PaymentSucceeded]]
    * event to the [[akka.actor.ActorSystem ActorSystem]]'s
    * [[akka.event.EventStream ActorSystem.eventStream]]
    *
    * If your application is interested in listening for payments,
    * you need to subscribe to the even stream and listen for a
    * [[org.bitcoins.eclair.rpc.json.PaymentSucceeded PaymentSucceeded]]
    * case class. You also need to check the
    * payment hash is the hash you expected
    */
  private def registerPaymentMonitor(invoice: LnInvoice)(
      implicit system: ActorSystem): Unit = {

    val p: Promise[Unit] = Promise[Unit]()

    val runnable = new Runnable() {

      override def run(): Unit = {
        val isPaidF = checkPayment(Left(invoice))

        //register callback that publishes a payment to our actor system's
        //event stream,
        isPaidF.map { isPaid: Boolean =>
          if (!isPaid) {
            //do nothing since the invoice has not been paid yet
            ()
          } else {
            //invoice has been paid, let's publish to event stream
            //so subscribers so the even stream can see that a payment
            //was received
            //we need to create a `PaymentSucceeded`
//            val ps = PaymentSucceeded(amountMsat = invoice.amount.get.toMSat,
//                                      paymentHash =
//                                        invoice.lnTags.paymentHash.hash,
//                                      paymentPreimage = "",
//                                      route = JsArray.empty)
//            system.eventStream.publish(ps)

            //complete the promise so the runnable will be canceled
            p.success(())

            ()
          }
        }

        ()
      }
    }

    val cancellable = system.scheduler.schedule(1.seconds, 1.seconds, runnable)

    p.future.map(_ => cancellable.cancel())

    ()
  }

  def receive(): Future[LnInvoice] =
    receive(amountMsat = None, description = None, expirySeconds = None)

  def receive(description: String): Future[LnInvoice] =
    receive(amountMsat = None, Some(description), expirySeconds = None)

  override def receive(
      amountMsat: LnCurrencyUnit,
      description: String): Future[LnInvoice] =
    receive(Some(amountMsat), Some(description), expirySeconds = None)

  def receive(
      amountMsat: LnCurrencyUnit,
      description: String,
      expirySeconds: Long): Future[LnInvoice] =
    receive(Some(amountMsat), Some(description), Some(expirySeconds))

  def receive(amountMsat: LnCurrencyUnit): Future[LnInvoice] =
    receive(Some(amountMsat), description = None, expirySeconds = None)

  def receive(
      amountMsat: LnCurrencyUnit,
      expirySeconds: Long): Future[LnInvoice] =
    receive(Some(amountMsat), description = None, Some(expirySeconds))

  override def sendToNode(nodeId: NodeId, amountMsat: LnCurrencyUnit, paymentHash: Sha256Digest, maxAttempts: Option[Int], feeThresholdSat: Option[LnCurrencyUnit], maxFeePct: Option[Int]): Future[String] = {
    val params = Seq(
      "nodeId" -> nodeId.toString,
      "amountMsat" -> amountMsat.toMSat.toBigDecimal.toString,
      "paymentHash" -> paymentHash.hex) ++ Seq(
      maxAttempts.map(x => "maxAttempts" -> x.toString),
      feeThresholdSat.map(x => "feeThresholdSat" -> x.toSatoshis.toBigDecimal.toString),
      maxFeePct.map(x => "maxFeePct" -> x.toString)
    ).flatten

    eclairCallNew[String]("sendtonode", params: _*)
  }

  def sendToRoute(route: TraversableOnce[NodeId], amountMsat: LnCurrencyUnit, paymentHash: Sha256Digest, finalCltvExpiry: Long): Future[String] = {
    eclairCallNew[String]("sendtoroute",
      "route" -> route.mkString(","),
      "amountMsat" -> amountMsat.toMSat.toBigDecimal.toString,
      "paymentHash" -> paymentHash.hex,
      "finalCltvExpiry" -> finalCltvExpiry.toString)
  }

  private def send(
      invoice: LnInvoice,
      amountMsat: Option[LnCurrencyUnit]): Future[PaymentResult] = {

    val params = {
      if (amountMsat.isEmpty) {
        List(JsString(invoice.toString))
      } else {
        List(JsString(invoice.toString), JsNumber(amountMsat.get.toMSat.toLong))
      }
    }

    eclairCall[PaymentResult]("send", params)
  }

  def send(invoice: LnInvoice): Future[PaymentResult] = send(invoice, None)

  def send(
      invoice: LnInvoice,
      amountMsat: LnCurrencyUnit): Future[PaymentResult] =
    send(invoice, Some(amountMsat))

  override def updateRelayFee(
      channelId: ChannelId,
      feeBaseMsat: MilliSatoshis,
      feeProportionalMillionths: Long): Future[Unit] = {
    eclairCallNew[Unit]("updaterelayfee",
      "channelId" -> channelId.hex,
      "feeBaseMsat" -> feeBaseMsat.toLong.toString,
      "feeProportionalMillionths" -> feeProportionalMillionths.toString)
  }

  override def updateRelayFee(
      shortChannelId: ShortChannelId,
      feeBaseMsat: MilliSatoshis,
      feeProportionalMillionths: Long): Future[Unit] = {
    eclairCallNew[Unit]("updaterelayfee",
      "shortChannelId" -> shortChannelId.toHumanReadableString,
      "feeBaseMsat" -> feeBaseMsat.toLong.toString,
      "feeProportionalMillionths" -> feeProportionalMillionths.toString)
  }

  // TODO: channelstats, audit, networkfees?
  // TODO: Add types

  private def eclairCallNew[T](command: String, parameters: (String, String)*)(
      implicit
      reader: Reads[T]): Future[T] = {
    val request = buildRequestNew(getDaemon, command, parameters: _*)

    logger.trace(s"eclair rpc call ${request}")
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)
    payloadF.map { payload =>
//      try {
        val validated: JsResult[T] = payload.validate[T]
        val parsed: T = parseResult(validated, payload, command)
        parsed
//      } catch {
//        case e: Throwable =>
//          println(command)
//          println(payload)
//          e.printStackTrace()
//          throw e
//      }
    }
  }

  private def eclairCall[T](
      command: String,
      parameters: List[JsValue] = List.empty)(
      implicit
      reader: Reads[T]): Future[T] = {
    val request = buildRequest(getDaemon, command, JsArray(parameters))

    logger.trace(s"eclair rpc call ${request}")
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)
    payloadF.map { payload =>
      val validated: JsResult[T] = (payload \ resultKey).validate[T]
      val parsed: T = parseResult(validated, payload, command)
      parsed
    }
  }

  case class RpcError(error: String)
  implicit val rpcErrorReads: Reads[RpcError] = Json.reads[RpcError]

  private def parseResult[T](
      result: JsResult[T],
      json: JsValue,
      commandName: String): T = {
    result match {
      case res: JsSuccess[T] =>
        res.value
      case res: JsError =>
        json.validate[RpcError] match {
          case err: JsSuccess[RpcError] =>
            val datadirMsg = instance.authCredentials.datadir
              .map(d => s"datadir=${d}")
              .getOrElse("")
            val errMsg =
              s"Error for command=${commandName} ${datadirMsg}, ${err.value.error}"
            logger.error(errMsg)
            throw new RuntimeException(errMsg)
          case _: JsError =>
            logger.error(JsError.toJson(res).toString())
            throw new IllegalArgumentException(
              s"Could not parse JsResult! JSON: ${json}")
        }
    }
  }

  private def getPayload(response: HttpResponse): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _)

    payloadF.map { payload =>
      val parsed: JsValue = Json.parse(payload.decodeString(ByteString.UTF_8))
      parsed
    }
  }

  private def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    val respF = Http(m.system).singleRequest(req)
    respF
  }

  private def buildRequestNew(
      instance: EclairInstance,
      methodName: String,
      params: (String, String)*): HttpRequest = {

    val uri = instance.rpcUri.resolve("/" + methodName).toString
    // Eclair doesn't use a username
    val username = ""
    val password = instance.authCredentials.password
    HttpRequest(method = HttpMethods.POST,
                uri,
                entity = FormData(params: _*).toEntity)
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }

  private def buildRequest(
      instance: EclairInstance,
      methodName: String,
      params: JsArray): HttpRequest = {
    val uuid = UUID.randomUUID().toString

    val obj: JsObject = JsObject(
      Map("method" -> JsString(methodName),
          "params" -> params,
          "id" -> JsString(uuid)))

    val uri = instance.rpcUri.toString
    // Eclair doesn't use a username
    val username = ""
    val password = instance.authCredentials.password
    HttpRequest(method = HttpMethods.POST,
                uri,
                entity =
                  HttpEntity(ContentTypes.`application/json`, obj.toString))
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }

  private def pathToEclairJar: String = {
    val path = Properties
      .envOrNone("ECLAIR_PATH")
      .getOrElse(throw new RuntimeException(
        List("Environment variable ECLAIR_PATH is not set!",
             "This needs to be set to the directory containing the Eclair Jar")
          .mkString(" ")))

    //val eclairV = "/eclair-node-0.2-beta8-52821b8.jar"
    val eclairV = "/eclair-node-0.3-a5debcd.jar"
    val fullPath = path + eclairV

    val jar = new File(fullPath)
    if (jar.exists) {
      fullPath
    } else {
      throw new RuntimeException(s"Could not Eclair Jar at location $fullPath")
    }
  }

  private var process: Option[Process] = None

  /** Starts eclair on the local system.
    *
    * @return a future that completes when eclair is fully started.
    *         If eclair has not successfully started in 60 seconds
    *         the future times out.
    */
  def start(): Future[Unit] = {

    val _ = {

      require(instance.authCredentials.datadir.isDefined,
              s"A datadir needs to be provided to start eclair")

      if (process.isEmpty) {
        val p = Process(
          s"java -jar -Declair.datadir=${instance.authCredentials.datadir.get} $pathToEclairJar &")
        val result = p.run()
        logger.debug(
          s"Starting eclair with datadir ${instance.authCredentials.datadir.get}")

        process = Some(result)
        ()
      } else {
        logger.info(s"Eclair was already started!")
        ()
      }
    }

    val started = AsyncUtil.retryUntilSatisfiedF(() => isStarted,
                                                 duration = 1.seconds,
                                                 maxTries = 60)

    started
  }

  def isStarted(): Future[Boolean] = {
    val p = Promise[Boolean]()

    getInfo.onComplete {
      case Success(_) =>
        p.success(true)
      case Failure(_) =>
        p.success(false)
    }

    p.future
  }

  def stop(): Option[Unit] = {
    process.map(_.destroy())
  }
}
