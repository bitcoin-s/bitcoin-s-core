package org.bitcoins.tor.config

import com.typesafe.config.Config
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory, ConfigOps}
import org.bitcoins.core.api.CallbackConfig
import org.bitcoins.core.api.tor.Socks5ProxyParams
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.tor.TorProtocolHandler.{Password, SafeCookie}
import org.bitcoins.tor.client.TorClient
import org.bitcoins.tor.{TorCallbacks, TorParams}

import java.io.File
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class TorAppConfig(
    baseDatadir: Path,
    private val subModuleNameOpt: Option[String],
    configOverrides: Vector[Config])(implicit ec: ExecutionContext)
    extends AppConfig
    with CallbackConfig[TorCallbacks] {

  override protected[bitcoins] def moduleName: String = TorAppConfig.moduleName
  override protected[bitcoins] type ConfigType = TorAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Vector[Config]): TorAppConfig =
    TorAppConfig(baseDatadir, subModuleNameOpt, configs)

  override lazy val callbackFactory: TorCallbacks.type = TorCallbacks
  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  lazy val torDir: Path = baseDatadir.resolve("tor")

  lazy val torLogFile: Path = torDir.resolve("TorLogs.txt")

  lazy val torProvided = getBoolean("tor.provided")

  lazy val useRandomPorts = getBoolean("tor.use-random-ports")

  lazy val targets = getStringList("tor.targets")
    .map(NetworkUtil.parseInetSocketAddress(_, -1))

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] = {
    if (getBoolean("proxy.enabled")) {
      val address = if (torProvided) {
        NetworkUtil.parseInetSocketAddress(getString("proxy.socks5"),
                                           TorParams.DefaultProxyPort)
      } else {
        new InetSocketAddress(InetAddress.getLoopbackAddress,
                              if (useRandomPorts)
                                TorAppConfig.randomSocks5Port
                              else TorParams.DefaultProxyPort)
      }
      Some(
        Socks5ProxyParams(
          address = address,
          credentialsOpt = None,
          randomizeCredentials = true
        )
      )
    } else {
      None
    }
  }

  lazy val torParams: Option[TorParams] = {
    if (getBoolean("tor.enabled")) {
      val address = if (torProvided) {
        NetworkUtil.parseInetSocketAddress(getString("tor.control"),
                                           TorParams.DefaultControlPort)
      } else {
        new InetSocketAddress(InetAddress.getLoopbackAddress,
                              if (useRandomPorts)
                                TorAppConfig.randomControlPort
                              else TorParams.DefaultControlPort)
      }

      val auth = getStringOrNone("tor.password") match {
        case Some(pass) => Password(pass)
        case None       => SafeCookie()
      }

      val privKeyPath =
        getStringOrNone("tor.privateKeyPath") match {
          case Some(path) => new File(path).toPath
          case None       => datadir.resolve("tor_priv_key")
        }

      Some(TorParams(address, auth, privKeyPath))
    } else {
      None
    }
  }

  lazy val enabled: Boolean = socks5ProxyParams.isDefined || torParams.isDefined

  private val isBootstrappedLogLine = "Bootstrapped 100% (done): Done"

  def createClient(implicit ec: ExecutionContext): TorClient = {
    new TorClient()(ec, this)
  }

  /** Ensures correct tables and other required information is in
    * place for our node.
    */
  override def start(): Future[Unit] = {
    val f = if (torProvided) {
      logger.info(s"Tor provided to us, skipping start")
      Future.unit
    } else {
      lazy val torRunning = checkIfTorAlreadyRunning
      if (enabled && !isStarted.get && !torRunning) {
        isStarted.set(true)
        logger.info(s"Starting Tor daemon")
        val start = System.currentTimeMillis()
        //remove old tor log file so we accurately tell when
        //the binary is started, if we don't remove this
        //we could have that log line appear from previous runs
        if (torLogFile.toFile.exists()) {
          torLogFile.toFile.delete()
        }
        val client = createClient

        for {
          _ <- client.startBinary()
          _ = Runtime.getRuntime.addShutdownHook(new Thread() {
            override def run(): Unit = {
              // don't forget to stop the daemon on exit
              Await.result(client.stopBinary(), 30.seconds)
            }
          })
          _ <- isBinaryFullyStarted()
        } yield {
          logger.info(
            s"Tor daemon is fully started, it took=${System.currentTimeMillis() - start}ms")
        }
      } else if (isStarted.get) {
        logger.debug(s"Tor daemon already started")
        Future.unit
      } else if (torRunning) {
        logger.warn(
          s"Tor daemon was requested to start, but it is already running. Not starting tor")
        Future.unit
      } else {
        logger.warn(
          s"Tor daemon was requested to start, but it is disabled in the configuration file. Not starting tor")
        Future.unit
      }
    }
    f.failed.foreach(err => logger.error("Error starting TorAppConfig", err))
    f
  }

  override def stop(): Future[Unit] = {
    clearCallbacks()
    if (torProvided) {
      Future.unit
    } else {
      createClient.stopBinary().map(_ => isStarted.set(false))
    }
  }

  /** Checks if the tor binary is started by looking for a log in the [[torLogFile]]
    * The log line we are looking or is
    * {{{
    *     Bootstrapped 100% (done): Done
    *  }}}
    */
  private def isBinaryFullyStarted(): Future[Unit] = {
    //tor can take at least 25 seconds to start at times
    //see: https://github.com/bitcoin-s/bitcoin-s/pull/3558#issuecomment-899819698
    AsyncUtil
      .retryUntilSatisfied(checkIfLogExists, 1.second, 120)
      //execute started callbacks
      .flatMap(_ => callBacks.executeOnTorStarted())
      .recover { case _: AsyncUtil.RpcRetryException =>
        throw new RuntimeException(
          s"Could not start tor, please try again in a few minutes")
      }
  }

  /** Checks it the [[isBootstrappedLogLine]] exists in the tor log file */
  private def checkIfLogExists: Boolean = {
    torLogFile.toFile.exists() && {
      val stream = Files.lines(torLogFile)
      try {
        stream
          .filter { case line: String =>
            line.contains(isBootstrappedLogLine)
          }
          .count() > 0
      } finally if (stream != null) stream.close()
    }
  }

  private def checkIfTorAlreadyRunning: Boolean = {
    val toCheck = socks5ProxyParams match {
      case Some(params) => params.address
      case None =>
        torParams match {
          case Some(params) => params.controlAddress
          case None =>
            new InetSocketAddress(InetAddress.getLoopbackAddress,
                                  TorParams.DefaultProxyPort)
        }
    }

    NetworkUtil.portIsBound(toCheck)
  }

  private def getBoolean(key: String): Boolean =
    getConfigValue(config.getBoolean)(key)

  private def getString(key: String): String =
    getConfigValue(config.getString)(key)

  private def getStringOrNone(key: String): Option[String] =
    getConfigValue(config.getStringOrNone)(key)

  private def getStringList(key: String): Vector[String] = try {
    val list = getConfigValue(config.getStringList)(key)
    0.until(list.size())
      .foldLeft(Vector.empty[String])((acc, i) => acc :+ list.get(i))
      .flatMap(_.split(","))
  } catch {
    case _: com.typesafe.config.ConfigException.Missing => Vector()
  }

  private def getConfigValue[V](getValue: String => V)(key: String): V = {
    subModuleNameOpt match {
      case Some(subModuleName) =>
        val path = s"bitcoin-s.$subModuleName.$key"
        if (config.hasPath(path)) {
          getValue(path)
        } else {
          getValue(s"bitcoin-s.$key")
        }
      case None =>
        getValue(s"bitcoin-s.$key")
    }
  }
}

object TorAppConfig extends AppConfigFactory[TorAppConfig] {

  override val moduleName: String = "tor"

  /** Constructs a tor configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): TorAppConfig =
    TorAppConfig(datadir, None, confs)

  lazy val randomSocks5Port: Int = ports.proxyPort

  lazy val randomControlPort: Int = ports.controlPort

  private case class TorPorts(proxyPort: Int, controlPort: Int)

  private lazy val ports = {
    val proxyPort = NetworkUtil.randomPort()
    var controlPort = proxyPort
    while (proxyPort == controlPort) {
      controlPort = NetworkUtil.randomPort()
    }
    TorPorts(proxyPort, controlPort)
  }

}
