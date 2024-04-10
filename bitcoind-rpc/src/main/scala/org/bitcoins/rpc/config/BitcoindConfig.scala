package org.bitcoins.rpc.config

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.commons.ConfigFactory
import org.bitcoins.core.config._

import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.file.{Files, Path, Paths}
import scala.util.Properties

/** This class represents a parsed `bitcoin.conf` file. It
  * respects the different ways of writing options in
  * `bitcoin.conf`: Raw options, network-prefixed options
  * and options within network sections. It also tries to
  * conform to the way Bitcoin Core gives precedence to the
  * different properties.
  *
  * Not all options are exposed from this class. We only
  * expose those that are of relevance when making RPC
  * requests.
  *
  * @see https://github.com/bitcoin/bitcoin/blob/master/doc/bitcoin-conf.md
  */
case class BitcoindConfig(
    private[bitcoins] val lines: Seq[String],
    datadir: File)
    extends BitcoinSLogger {

  //create datadir and config if it DNE on disk
  if (!datadir.exists()) {
    logger.debug(
      s"datadir=${datadir.getAbsolutePath} does not exist, creating now")
    datadir.mkdirs()
    BitcoindConfig.writeConfigToFile(this, datadir)
  }

  private val confFile = datadir.toPath.resolve("bitcoin.conf")

  //create bitcoin.conf file in datadir if it does not exist
  if (!Files.exists(confFile)) {
    logger.debug(
      s"bitcoin.conf in datadir=${datadir.getAbsolutePath} does not exist, creating now")
    BitcoindConfig.writeConfigToFile(this, datadir)
  }

  /** Converts the config back to a string that can be written
    * to file, and passed to `bitcoind`
    */
  lazy val toWriteableString: String = lines.mkString("\n")

  /** The optional index of the first header section encountered */
  private lazy val firstHeaderSectionIndex: Option[Int] = {
    val indices =
      List(RegTest, TestNet3, MainNet).flatMap(headerSectionIndex)
    if (indices.nonEmpty) Some(indices.min) else None
  }

  /** The optional index of the header section for the given network */
  private def headerSectionIndex(network: NetworkParameters): Option[Int] =
    lines.indexOf(s"[${networkString(network)}]") match {
      case -1         => None
      case other: Int => Some(other)
    }

  private def networkString(network: NetworkParameters) =
    network match {
      case MainNet  => "main"
      case RegTest  => "regtest"
      case TestNet3 => "test"
      case SigNet   => "signet"
    }

  /** The networks we're _not_ on */
  private lazy val otherNetworks = network match {
    case MainNet  => List("test", "regtest", "signet")
    case RegTest  => List("main", "test", "signet")
    case TestNet3 => List("main", "regtest", "signet")
    case SigNet   => List("main", "test", "regtest")
  }

  private lazy val ourNetworkString: String = networkString(network)

  /** Splits the provided lines into pairs of keys/values
    * based on `=`, and then applies the provided
    * `collect` function on those pairs
    */
  private def collectFrom(lines: Seq[String])(
      collect: PartialFunction[(String, String), String]): Seq[String] = {

    val splittedPairs = {
      val splitLines = lines.map(
        _.split("=")
          .map(_.trim)
          .toList)

      splitLines.collect { case h :: t :: _ =>
        h -> t
      }
    }

    splittedPairs.collect(collect)
  }

  /** Applies the given partial function to all key/value pairs
    * found in this config
    */
  private val collectAllLines: PartialFunction[(String, String), String] => Seq[
    String] = collectFrom(lines)(_)

  /** The blockchain network associated with this `bitcoind` config */
  lazy val network: NetworkParameters = {
    val networkStrOpt =
      collectAllLines {
        case (network @ ("testnet" | "regtest" | "mainnet"), "1") => network
      }.lastOption

    val networkOpt = networkStrOpt.flatMap(Networks.fromStringOpt)

    (networkOpt, networkStrOpt) match {
      case (None, Some(badStr)) =>
        logger.warn(
          s"'$badStr' is not a valid Bitcoin network! Defaulting to mainnet")
      case _ =>
    }

    networkOpt.getOrElse(MainNet)
  }

  /** First searches for option prefixed with network,
    * then section with header
    * and lastly just raw option
    */
  private[config] def getValue(key: String): Option[String] =
    readPrefixOpt(key)
      .orElse(readSectionHeaderOpt(key))
      .orElse(readRawOpt(key))

  /** Searches the config for a key matching the provided
    * string that's prefixed by the network we're currently
    * on
    */
  private def readPrefixOpt(key: String): Option[String] = {
    val prefixedOptKey = s"$ourNetworkString.$key"
    val collect = firstHeaderSectionIndex match {
      case None        => collectAllLines(_)
      case Some(index) => collectFrom(lines.take(index))(_)
    }
    collect { case (`prefixedOptKey`, value) =>
      value
    }.headOption
  }

  /** Searches the config for a key matching the provided
    * string that's under a section header matching the
    * network we're on.
    */
  private def readSectionHeaderOpt(key: String): Option[String] = {
    val startIndexOpt = lines.indexOf(s"[$ourNetworkString]") match {
      case -1 =>
        None
      case other: Int => Some(other)
    }

    for {
      startIndex <- startIndexOpt
      endIndex = {
        // we're looking for the first section _after_ the
        // section for our current network
        val toSearchIn = lines.zipWithIndex.drop(startIndex)
        toSearchIn
          .find { case (line, _) =>
            otherNetworks.exists(net => s"[$net]" == line)
          }
          .map { case (_, index) =>
            index
          }
          // we got to the end without finding a new section header,
          // that means we can search to the bottom
          .getOrElse(lines.length)
      }
      result <- {
        val linesToSearchIn = lines.slice(startIndex, endIndex)
        val collect = collectFrom(linesToSearchIn)(_)
        collect { case (`key`, value) =>
          value
        }.headOption
      }
    } yield result
  }

  /** Searches the config for a key matching the provided
    * string that's _not_ in a section header or prefixed
    * by a network.
    */
  private def readRawOpt(key: String): Option[String] = {
    val linesToSearchIn = lines.takeWhile(!_.startsWith("["))
    val collect = collectFrom(linesToSearchIn)(_)
    collect { case (`key`, value) =>
      value
    }.headOption
  }

  /** Converts a string to an InetSocketAddress */
  private def toInetSocketAddress(string: String): InetSocketAddress = {
    val uri = new URI(string)
    new InetSocketAddress(uri.getHost, uri.getPort)
  }

  lazy val username: Option[String] = getValue("rpcuser")
  lazy val password: Option[String] = getValue("rpcpassword")

  lazy val zmqpubrawblock: Option[InetSocketAddress] =
    getValue("zmqpubrawblock").map(toInetSocketAddress)

  lazy val zmqpubrawtx: Option[InetSocketAddress] =
    getValue("zmqpubrawtx").map(toInetSocketAddress)

  lazy val zmqpubhashblock: Option[InetSocketAddress] =
    getValue("zmqpubhashblock").map(toInetSocketAddress)

  lazy val zmqpubhashtx: Option[InetSocketAddress] =
    getValue("zmqpubhashtx").map(toInetSocketAddress)

  lazy val port: Int = getValue("port").map(_.toInt).getOrElse(network.port)

  /** Defaults to localhost */
  lazy val bind: URI = new URI({
    val baseUrl = getValue("bind").getOrElse("localhost")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl

  })

  lazy val uri: URI = new URI(s"$bind:$port")

  lazy val rpcport: Int =
    getValue("rpcport").map(_.toInt).getOrElse(network.rpcPort)

  /** Defaults to localhost */
  lazy val rpcbind: URI = new URI({
    val baseUrl = getValue("rpcbind").getOrElse("localhost")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })
  lazy val rpcUri: URI = new URI(s"$rpcbind:$rpcport")

  /** Creates a new config with the given keys and values appended */
  def withOption(key: String, value: String): BitcoindConfig = {
    val ourLines = this.lines
    val newLine = s"$key=$value"
    val lines = newLine +: ourLines
    val newConfig = BitcoindConfig(lines, datadir)
    logger.debug(
      s"Appending new config with $key=$value to datadir=${datadir.getAbsolutePath}")
    BitcoindConfig.writeConfigToFile(newConfig, datadir)

    newConfig
  }

  /** Creates a new config with the given key and values,
    * with the given network prefixed to the key
    *
    * Old config:
    * {{{
    * rpcport=4000
    * }}}
    *
    * New config:
    * {{{
    * withOption("rpcport", "2000", MainNet) =
    * main.rpcport=2000
    * rpcport=4000
    * }}}
    */
  def withOption(
      key: String,
      value: String,
      network: NetworkParameters): BitcoindConfig =
    withOption(key = s"${networkString(network)}.$key", value = value)

  def withDatadir(newDatadir: File): BitcoindConfig = {
    BitcoindConfig(lines, newDatadir)
  }

}

object BitcoindConfig
    extends ConfigFactory[BitcoindConfig]
    with BitcoinSLogger {

  /** The empty `bitcoind` config */
  lazy val empty: BitcoindConfig = BitcoindConfig("", DEFAULT_DATADIR)

  /** Constructs a `bitcoind` config from the given string,
    * by splitting it on newlines
    */
  override def apply(config: String, datadir: File): BitcoindConfig =
    apply(config.split("\n").toList, datadir)

  /** Reads the given path and construct a `bitcoind` config from it */
  override def apply(config: Path): BitcoindConfig =
    apply(config.toFile, config.getParent.toFile)

  /** Reads the given file and construct a `bitcoind` config from it */
  override def apply(
      config: File,
      datadir: File = DEFAULT_DATADIR): BitcoindConfig = {
    import scala.jdk.CollectionConverters.IteratorHasAsScala
    val lines = Files
      .readAllLines(config.toPath)
      .iterator()
      .asScala
      .toList

    apply(lines, datadir)
  }

  override def fromConfigFile(file: File): BitcoindConfig = {
    apply(file.toPath)
  }

  override def fromDataDir(dir: File): BitcoindConfig = {
    apply(dir.toPath.resolve("bitcoin.conf"))
  }

  /** If there is a `bitcoin.conf` in the default
    * data directory, this is read. Otherwise, the
    * default configuration is returned.
    */
  override def fromDefaultDatadir: BitcoindConfig = {
    if (DEFAULT_CONF_FILE.isFile) {
      apply(DEFAULT_CONF_FILE)
    } else {
      BitcoindConfig.empty
    }
  }

  /** @see https://en.bitcoin.it/wiki/Data_directory
    */
  override val DEFAULT_DATADIR: File = {
    val path = if (Properties.isMac) {
      Paths.get(Properties.userHome,
                "Library",
                "Application Support",
                "Bitcoin")
    } else if (Properties.isWin) {
      Paths.get("C:",
                "Users",
                Properties.userName,
                "Appdata",
                "Roaming",
                "Bitcoin")
    } else {
      Paths.get(Properties.userHome, ".bitcoin")
    }
    path.toFile
  }

  /** Default location of bitcoind conf file */
  override val DEFAULT_CONF_FILE: File = DEFAULT_DATADIR.toPath
    .resolve("bitcoin.conf")
    .toFile

  /** Writes the config to the data directory within it, if it doesn't
    * exist. Returns the written file.
    */
  override def writeConfigToFile(
      config: BitcoindConfig,
      datadir: File): Path = {

    val confStr = config.lines.mkString("\n")

    Files.createDirectories(datadir.toPath)
    val confFile = datadir.toPath.resolve("bitcoin.conf")

    if (datadir == DEFAULT_DATADIR && confFile.toFile == DEFAULT_CONF_FILE) {
      logger.warn(
        s"We will not overwrite the existing bitcoin.conf in default datadir")
    } else {
      Files.write(confFile, confStr.getBytes)
    }

    confFile
  }
}
