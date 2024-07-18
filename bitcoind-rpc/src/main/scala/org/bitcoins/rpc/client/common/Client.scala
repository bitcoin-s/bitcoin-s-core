package org.bitcoins.rpc.client.common

import com.fasterxml.jackson.core.JsonParseException
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.javadsl.model.headers.HttpCredentials
import org.apache.pekko.http.scaladsl.{Http, HttpExt}
import org.apache.pekko.http.scaladsl.model.{
  ContentTypes,
  HttpEntity,
  HttpMethods,
  HttpRequest,
  HttpResponse
}
import org.apache.pekko.http.scaladsl.settings.ConnectionPoolSettings
import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
import org.apache.pekko.stream.StreamTcpException
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.util.{BitcoinSLogger, NativeProcessFactory}
import org.bitcoins.core.config._
import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.{ECPrivateKey, ECPrivateKeyBytes}
import org.bitcoins.commons.rpc.BitcoindException
import org.bitcoins.rpc.config.BitcoindAuthCredentials.{
  CookieBased,
  PasswordBased
}
import org.bitcoins.rpc.config.{
  BitcoindAuthCredentials,
  BitcoindInstance,
  BitcoindInstanceLocal,
  BitcoindInstanceRemote
}
import org.bitcoins.tor.Socks5ClientTransport
import play.api.libs.json._

import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/** This is the base trait for Bitcoin Core RPC clients. It defines no RPC calls
  * except for the a ping. It contains functionality and utilities useful when
  * working with an RPC client, like data directories, log files and whether or
  * not the client is started.
  */
trait Client
    extends BitcoinSLogger
    with StartStopAsync[BitcoindRpcClient]
    with NativeProcessFactory {
  def version: Future[BitcoindVersion]
  protected val instance: BitcoindInstance

  protected def walletExtension(walletName: String): String =
    s"/wallet/$walletName"

  /** The log file of the Bitcoin Core daemon. This returns the log file if the
    * underlying instance is [[org.bitcoins.rpc.config.BitcoindInstanceLocal]],
    * and None if the underlying instance is [[BitcoindInstanceRemote]]
    */
  lazy val logFileOpt: Option[Path] = {
    instance match {
      case _: BitcoindInstanceRemote => None
      case local: BitcoindInstanceLocal =>
        val prefix = instance.network match {
          case MainNet  => ""
          case TestNet3 => "testnet"
          case RegTest  => "regtest"
          case SigNet   => "signet"
        }
        val path = local.datadir.toPath.resolve(prefix).resolve("debug.log")
        Some(path)
    }
  }

  /** The configuration file of the Bitcoin Core daemon This returns the conf
    * file is the underlying instance is [[BitcoindInstanceLocal]] and None if
    * the underlying instance is [[BitcoindInstanceRemote]]
    */
  lazy val confFileOpt: Option[Path] = {
    instance match {
      case _: BitcoindInstanceRemote =>
        None
      case local: BitcoindInstanceLocal =>
        val path = local.datadir.toPath.resolve("bitcoin.conf")
        Some(path)
    }
  }

  implicit protected val system: ActorSystem

  implicit override protected val executionContext: ExecutionContext =
    system.getDispatcher
  implicit protected val network: NetworkParameters = instance.network

  /** This is here (and not in JsonWrriters) so that the implicit network val is
    * accessible
    */
  implicit object ECPrivateKeyWrites extends Writes[ECPrivateKey] {

    override def writes(o: ECPrivateKey): JsValue =
      JsString(ECPrivateKeyUtil.toWIF(o.toPrivateKeyBytes(), network))
  }

  implicit val eCPrivateKeyWrites: Writes[ECPrivateKey] = ECPrivateKeyWrites

  /** This is here (and not in JsonWrriters) so that the implicit network val is
    * accessible
    */
  implicit object ECPrivateKeyBytesWrites extends Writes[ECPrivateKeyBytes] {

    override def writes(o: ECPrivateKeyBytes): JsValue =
      JsString(ECPrivateKeyUtil.toWIF(o, network))
  }

  implicit val eCPrivateKeyBytesWrites: Writes[ECPrivateKeyBytes] =
    ECPrivateKeyBytesWrites

  implicit val importMultiAddressWrites: Writes[RpcOpts.ImportMultiAddress] =
    Json.writes[RpcOpts.ImportMultiAddress]

  implicit val importMultiRequestWrites: Writes[RpcOpts.ImportMultiRequest] =
    Json.writes[RpcOpts.ImportMultiRequest]
  private val resultKey: String = "result"
  private val errorKey: String = "error"

  def getDaemon: BitcoindInstance = instance

  override lazy val cmd: Vector[String] = {
    instance match {
      case _: BitcoindInstanceRemote =>
        logger.warn(
          s"Cannot start remote instance with local binary command. You've likely misconfigured something"
        )
        Vector.empty
      case local: BitcoindInstanceLocal =>
        val binaryPath = local.binary.getAbsolutePath
        val cmd = Vector(
          binaryPath,
          "-datadir=" + local.datadir
        )
        logger.debug(
          s"starting bitcoind with datadir ${local.datadir} and binary path $binaryPath"
        )
        // println(s"cmd=$cmd")
        cmd

    }
  }

  /** Starts bitcoind on the local system.
    * @return
    *   a future that completes when bitcoind is fully started. This future
    *   times out after 60 seconds if the client cannot be started
    */
  override def start(): Future[BitcoindRpcClient] = {
    logger.info(s"Client.start() instance=$instance")
    // if we're doing cookie based authentication, we might attempt
    // to read the cookie file before it's written. this ensures
    // we avoid that
    val awaitCookie: BitcoindAuthCredentials => Future[Unit] = {
      case cookie: CookieBased =>
        val cookieExistsF =
          AsyncUtil.retryUntilSatisfied(Files.exists(cookie.cookiePath))
        cookieExistsF.onComplete {
          case Failure(exception) =>
            logger.error(s"Cookie filed was never created! $exception")
          case _: Success[_] =>
        }
        cookieExistsF
      case _: PasswordBased => Future.unit

    }
    val isAlreadyStarted: Future[Boolean] = isStartedF

    val started: Future[Future[BitcoindRpcClient]] = isAlreadyStarted.map {
      case false =>
        instance match {
          case _: BitcoindInstanceRemote =>
            sys.error(
              s"Cannot start a remote instance, it needs to be started on the remote host machine"
            )
          case local: BitcoindInstanceLocal =>
            val versionCheckF = version.map { v =>
              if (v != BitcoindVersion.Unknown) {
                val foundVersion = local.getVersion
                if (foundVersion != v) {
                  throw new RuntimeException(
                    s"Wrong version for bitcoind RPC client! Expected $version, got $foundVersion"
                  )
                }
              }
            }

            val startedF = versionCheckF.flatMap(_ => startBinary())

            for {
              _ <- startedF
              _ <- awaitCookie(instance.authCredentials)
              _ = isStartedFlag.set(true)
              _ <- AsyncUtil.retryUntilSatisfiedF(
                () => isStartedF,
                interval = 1.seconds,
                maxTries = 120
              )
            } yield this.asInstanceOf[BitcoindRpcClient]
        }
      case true =>
        for {
          _ <- awaitCookie(instance.authCredentials)
          _ = isStartedFlag.set(true)
        } yield this.asInstanceOf[BitcoindRpcClient]
    }

    started.onComplete {
      case Success(_) => logger.debug(s"started bitcoind")
      case Failure(exc) =>
        logger.info(
          s"Could not start bitcoind instance! Message: ${exc.getMessage}"
        )
        // When we're unable to start bitcoind that's most likely
        // either a configuration error or bug in Bitcoin-S. In either
        // case it's much easier to debug this with conf and logs
        // dumped somewhere. Especially in tests this is
        // convenient, as our test framework deletes the data directories
        // of our instances. We don't want to do this on mainnet,
        // as both the logs and conf file most likely contain sensitive
        // information

        instance match {
          case _: BitcoindInstanceRemote =>
            ()
          case _: BitcoindInstanceLocal =>
            if (network != MainNet) {
              val tempfile = Files.createTempFile("bitcoind-log-", ".dump")
              val logfile = Files.readAllBytes(logFileOpt.get)
              Files.write(tempfile, logfile)
              logger.info(s"Dumped debug.log to $tempfile")

              val otherTempfile = Files.createTempFile("bitcoin-conf-", ".dump")
              val conffile = Files.readAllBytes(confFileOpt.get)
              Files.write(otherTempfile, conffile)
              logger.info(s"Dumped bitcoin.conf to $otherTempfile")
            }
        }

    }

    started.flatten
  }

  private def tryPing(): Future[Boolean] = {
    val request = buildRequest(instance, "ping", JsArray.empty)
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] =
      responseF.flatMap(getPayload(_))

    // Ping successful if no error can be parsed from the payload
    val parsedF: Future[Boolean] = payloadF.map { payload =>
      (payload \ errorKey).validate[BitcoindException] match {
        case _: JsSuccess[BitcoindException] => false
        case _: JsError                      => true
      }
    }

    parsedF.recover {
      case exc: StreamTcpException
          if exc.getMessage.contains("Connection refused") =>
        false
      case _: JsonParseException =>
        // see https://github.com/bitcoin-s/bitcoin-s/issues/527
        false
    }
  }

  private val isStartedFlag: AtomicBoolean = new AtomicBoolean(false)

  /** Checks whether the underlying bitcoind daemon is running
    */
  def isStartedF: Future[Boolean] = {

    instance.authCredentials match {
      case cookie: CookieBased if Files.notExists(cookie.cookiePath) =>
        // if the cookie file doesn't exist we're not started
        Future.successful(false)
      case (CookieBased(_, _) | PasswordBased(_, _)) =>
        instance match {
          case _: BitcoindInstanceRemote =>
            // we cannot check locally if it has been started
            // so best we can do is try to ping
            tryPing()
          case _: BitcoindInstanceLocal =>
            // check if the binary has been started
            // and then tryPing() if it has
            if (isStartedFlag.get) {
              tryPing()
            } else {
              Future.successful(false)
            }
        }

    }
  }

  /** Stop method for BitcoindRpcClient that is stopped, inherits from the
    * StartStop trait
    * @return
    *   A future stopped bitcoindRPC client
    */
  def stop(): Future[BitcoindRpcClient] = {
    for {
      _ <- bitcoindCall[String]("stop")
      _ = isStartedFlag.set(false)
      // do we want to call this right away?
      // i think bitcoind stops asynchronously
      // so it returns fast from the 'stop' rpc command
      _ <- stopBinary()
    } yield this.asInstanceOf[BitcoindRpcClient]
  }

  /** Checks whether the underlyind bitcoind daemon is stopped
    * @return
    *   A future boolean which represents isstopped or not
    */
  def isStoppedF: Future[Boolean] = {
    isStartedF.map(started => !started)
  }

  // This RPC call is here to avoid circular trait depedency
  def ping(): Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  protected def bitcoindCall[T](
      command: String,
      parameters: List[JsValue] = List.empty,
      printError: Boolean = true,
      uriExtensionOpt: Option[String] = None
  )(implicit reader: Reads[T]): Future[T] = {
    val request =
      buildRequest(instance, command, JsArray(parameters), uriExtensionOpt)
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] =
      responseF.flatMap(getPayload(_))

    payloadF.map { payload =>
      /** These lines are handy if you want to inspect what's being sent to and
        * returned from bitcoind before it's parsed into a Scala type. However,
        * there will sensitive material in some of those calls (private keys,
        * XPUBs, balances, etc). It's therefore not a good idea to enable this
        * logging in production.
        */
//      logger.info(
//        s"Command: $command ${parameters.map(_.toString).mkString(" ")}")
//      logger.info(s"Payload: \n${Json.prettyPrint(payload)}")
      parseResult(
        result = (payload \ resultKey).validate[T],
        json = payload,
        printError = printError,
        command = command
      )
    }
  }

  protected def buildRequest(
      instance: BitcoindInstance,
      methodName: String,
      params: JsArray,
      uriExtensionOpt: Option[String] = None
  ): HttpRequest = {
    val uuid = UUID.randomUUID().toString
    val m: Map[String, JsValue] = Map(
      "method" -> JsString(methodName),
      "params" -> params,
      "id" -> JsString(uuid)
    )

    val jsObject = JsObject(m)

    val uriExtension = uriExtensionOpt.getOrElse("")

    // Would toString work?
    val uri =
      "http://" + instance.rpcUri.getHost + ":" + instance.rpcUri.getPort + uriExtension
    val username = instance.authCredentials.username
    val password = instance.authCredentials.password
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity = HttpEntity(ContentTypes.`application/json`, jsObject.toString())
    )
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password)
      )
  }

  /** Cached http client to send requests to bitcoind with */
  private lazy val httpClient: HttpExt = Http(system)

  private lazy val httpConnectionPoolSettings: ConnectionPoolSettings = {
    instance match {
      case remote: BitcoindInstanceRemote =>
        Socks5ClientTransport.createConnectionPoolSettings(
          instance.rpcUri,
          remote.proxyParams
        )
      case _: BitcoindInstanceLocal =>
        Socks5ClientTransport.createConnectionPoolSettings(
          instance.rpcUri,
          None
        )
    }
  }

  protected def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    httpClient.singleRequest(req, settings = httpConnectionPoolSettings)
  }

  /** Parses the payload of the given response into JSON.
    *
    * The command, parameters and request are given as debug parameters, and
    * only used for printing diagnostics if things go belly-up.
    */
  protected def getPayload(response: HttpResponse): Future[JsValue] = {
    try {
      Unmarshal(response).to[String].map { data =>
        if (data.isEmpty) {
          throw new IllegalArgumentException(
            s"Bad authentication credentials supplied, cannot connect to bitcoind rpc"
          )
        } else {
          Json.parse(data)
        }
      }
    } catch {
      case NonFatal(exn) =>
        Future.failed(exn)
    }
  }

  // Should both logging and throwing be happening?
  private def parseResult[T](
      result: JsResult[T],
      json: JsValue,
      printError: Boolean,
      command: String
  ): T = {
    checkUnitError[T](result, json, printError)

    result match {
      case JsSuccess(value, _) => value
      case res: JsError =>
        (json \ errorKey).validate[BitcoindException] match {
          case JsSuccess(err, _) =>
            if (printError) {
              logger.error(s"parseResult.err=$err command=$command")
            }
            throw err
          case _: JsError =>
            val jsonResult = (json \ resultKey).get
            val errString =
              s"Error when parsing result of '$command': ${JsError.toJson(res).toString}!"
            if (printError) logger.error(errString + s"JSON: $jsonResult")
            throw new IllegalArgumentException(
              s"Could not parse JsResult: $jsonResult! Error: $errString"
            )
        }
    }
  }

  // Catches errors thrown by calls with Unit as the expected return type (which isn't handled by UnitReads)
  private def checkUnitError[T](
      result: JsResult[T],
      json: JsValue,
      printError: Boolean
  ): Unit = {
    if (result == JsSuccess(())) {
      (json \ errorKey).validate[BitcoindException] match {
        case JsSuccess(err, _) =>
          if (printError) {
            logger.error(s"$err")
          }
          throw err
        case _: JsError =>
      }
    }
  }

}
