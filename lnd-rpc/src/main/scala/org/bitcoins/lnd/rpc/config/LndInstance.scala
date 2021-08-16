package org.bitcoins.lnd.rpc.config

import org.bitcoins.core.api.commons.InstanceFactoryLocal
import org.bitcoins.core.config._
import org.bitcoins.rpc.config.BitcoindAuthCredentials._
import org.bitcoins.rpc.config._
import scodec.bits._

import java.io.File
import java.net._
import java.nio.file._
import scala.util.Properties

sealed trait LndInstance {
  def network: BitcoinNetwork
  def listenBinding: URI
  def restUri: URI
  def rpcUri: URI
  def bitcoindAuthCredentials: PasswordBased
  def bitcoindRpcUri: URI
  def zmqConfig: ZmqConfig
  def debugLevel: LogLevel

  private var macaroonOpt: Option[String] = None

  def datadir: Path

  def macaroon: String = {
    macaroonOpt match {
      case Some(value) => value
      case None =>
        val path =
          datadir
            .resolve("data")
            .resolve("chain")
            .resolve("bitcoin")
            .resolve(LndInstanceLocal.getNetworkDirName(network))
            .resolve("admin.macaroon")

        val bytes = Files.readAllBytes(path)
        val hex = ByteVector(bytes).toHex

        macaroonOpt = Some(hex)
        hex
    }
  }

  lazy val certFile: File =
    datadir.resolve("tls.cert").toFile
}

case class LndInstanceLocal(
    datadir: Path,
    network: BitcoinNetwork,
    listenBinding: URI,
    restUri: URI,
    rpcUri: URI,
    bitcoindAuthCredentials: PasswordBased,
    bitcoindRpcUri: URI,
    zmqConfig: ZmqConfig,
    debugLevel: LogLevel)
    extends LndInstance

object LndInstanceLocal extends InstanceFactoryLocal[LndInstanceLocal] {

  override val DEFAULT_DATADIR: Path = Paths.get(Properties.userHome, ".lnd")

  override val DEFAULT_CONF_FILE: Path = DEFAULT_DATADIR.resolve("lnd.conf")

  private[lnd] def getNetworkDirName(network: BitcoinNetwork): String = {
    network match {
      case _: MainNet  => "mainnet"
      case _: TestNet3 => "testnet"
      case _: RegTest  => "regtest"
      case _: SigNet   => "simnet"
    }
  }

  override def fromConfigFile(
      file: File = DEFAULT_CONF_FILE.toFile): LndInstanceLocal = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val config = LndConfig(file, file.getParentFile)

    fromConfig(config)
  }

  override def fromDataDir(
      dir: File = DEFAULT_DATADIR.toFile): LndInstanceLocal = {
    require(dir.exists, s"${dir.getPath} does not exist!")
    require(dir.isDirectory, s"${dir.getPath} is not a directory!")

    val confFile = dir.toPath.resolve("lnd.conf").toFile
    val config = LndConfig(confFile, dir)

    fromConfig(config)
  }

  def fromConfig(config: LndConfig): LndInstanceLocal = {
    config.lndInstance
  }
}
