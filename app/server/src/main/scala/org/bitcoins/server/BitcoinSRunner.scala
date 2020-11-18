package org.bitcoins.server

import java.nio.file.{Path, Paths}

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config._
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.AppConfig

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Properties

trait BitcoinSRunner extends BitcoinSLogger {

  protected def args: Array[String]

  def actorSystemName: String

  implicit lazy val system: ActorSystem = {
    val system = ActorSystem(actorSystemName, baseConfig)
    system.log.info("Akka logger started")
    system
  }
  implicit lazy val ec: ExecutionContext = system.dispatcher

  lazy val argsWithIndex: Vector[(String, Int)] = args.toVector.zipWithIndex

  lazy val rpcPortOpt: Option[Int] = {
    val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
    portOpt.map {
      case (_, idx) => args(idx + 1).toInt
    }
  }

  lazy val forceChainWorkRecalc: Boolean =
    args.exists(_.toLowerCase == "--force-recalc-chainwork")

  private lazy val dataDirIndexOpt: Option[(String, Int)] = {
    argsWithIndex.find(_._1.toLowerCase == "--datadir")
  }

  private lazy val datadirPath: Path = dataDirIndexOpt match {
    case None => AppConfig.DEFAULT_BITCOIN_S_DATADIR
    case Some((_, dataDirIndex)) =>
      val str = args(dataDirIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      Paths.get(usableStr)
  }

  private lazy val configIndexOpt: Option[Int] = {
    argsWithIndex.find(_._1.toLowerCase == "--conf").map(_._2)
  }

  val withDatadir: Config =
    ConfigFactory.parseString(s"bitcoin-s.datadir = $datadirPath")

  lazy val baseConfig: Config = configIndexOpt match {
    case None =>
      AppConfig
        .getBaseConfig(datadirPath)
        .withFallback(withDatadir)
        .resolve()
    case Some(configIndex) =>
      val str = args(configIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      val path = Paths.get(usableStr)
      ConfigFactory
        .parseFile(path.toFile)
        .resolve()
  }

  lazy val configDataDir: Path =
    Paths.get(baseConfig.getString("bitcoin-s.datadir"))

  private lazy val networkStr: String =
    baseConfig.getString("bitcoin-s.network")

  private lazy val network: BitcoinNetwork =
    BitcoinNetworks.fromString(networkStr)

  private lazy val datadir: Path = {
    lazy val lastDirname = network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }
    configDataDir.resolve(lastDirname)
  }

  def startup: Future[Unit]

  // start everything!
  final def run(): Unit = {
    // Properly set log location
    System.setProperty("bitcoins.log.location", datadir.toAbsolutePath.toString)

    val runner = startup
    runner.failed.foreach { err =>
      logger.error(s"Failed to startup server!", err)
      sys.exit(1)
    }(scala.concurrent.ExecutionContext.Implicits.global)
  }
}
