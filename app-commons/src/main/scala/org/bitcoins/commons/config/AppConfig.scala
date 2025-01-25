package org.bitcoins.commons.config

import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.config.*
import org.bitcoins.core.protocol.blockchain.BitcoinChainParams
import org.bitcoins.core.util.{EnvUtil, StartStopAsync}

import java.nio.file.*
import scala.concurrent.Future
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Properties
import scala.util.matching.Regex

/** Everything needed to configure functionality of bitcoin-s applications is
  * found in here.
  *
  * @see
  *   [[https://github.com/bitcoin-s/bitcoin-s-core/blob/master/doc/configuration.md `configuration.md`]]
  *   for more information.
  */
abstract class AppConfig extends StartStopAsync[Unit] with BitcoinSLogger {

  /** Starts this project. After this future resolves, all operations should be
    * able to be performed correctly.
    *
    * Starting may include creating database tables, making directories or files
    * needed later or something else entirely.
    */
  override def start(): Future[Unit] = {
    Future.unit
  }

  /** Sub members of AppConfig should override this type with the type of
    * themselves, ensuring `withOverrides` return the correct type
    */
  protected[bitcoins] type ConfigType <: AppConfig

  /** Constructor to make a new instance of this config type */
  protected[bitcoins] def newConfigOfType(
      configOverrides: Vector[Config]
  ): ConfigType

  /** List of user-provided configs that should override defaults
    */
  protected[bitcoins] def configOverrides: Vector[Config]

  def withOverrides(configOverrides: Config): ConfigType = {
    withOverrides(Vector(configOverrides))
  }

  /** This method returns a new `AppConfig`, where every key under `bitcoin-s`
    * overrides the configuration picked up by other means (the `reference.conf`
    * provided by bitcoin-s and the `application.conf` provided by the user). If
    * you pass in configs with overlapping keys (e.g. several configs with the
    * key `bitcoin-s.network`), the latter config overrides the first.
    */
  def withOverrides(configOverrides: Vector[Config]): ConfigType = {
    val numOverrides = configOverrides.length
    if (logger.isDebugEnabled()) {
      // force lazy evaluation before we print
      // our lines
      val oldConfStr = this.config.asReadableJson

      logger.trace(s"Creating AppConfig with $numOverrides override(s) ")
      logger.trace(s"Old config:")
      logger.trace(oldConfStr)
    }

    if (logger.isTraceEnabled()) {
      configOverrides.zipWithIndex.foreach { case (c, idx) =>
        logger.trace(s"Override no. $idx: ${c.asReadableJson}")
      }
    }
    val newConf = {
      // the idea here is that after resolving the configuration,
      // we extract the value under the 'bitcoin-s' key and use
      // that as our config. here we have to do the reverse, to
      // get the keys to resolve correctly
      val reconstructedStr = s"""
      "bitcoin-s": ${this.config.getConfig("bitcoin-s").asReadableJson}
      """
      val reconstructed = ConfigFactory.parseString(reconstructedStr)
      newConfigOfType(reconstructed +: configOverrides)
    }

    // to avoid non-necessary lazy load
    if (logger.isDebugEnabled()) {
      // force lazy load before we print
      val newConfStr = newConf.config.asReadableJson

      logger.trace("New config:")
      logger.trace(newConfStr)
    }

    newConf
  }

  /** Name of the module. `chain`, `wallet`, `node` etc.
    */
  private[bitcoins] def moduleName: String

  /** Chain parameters for the blockchain we're on */
  lazy val chain: BitcoinChainParams = {
    val networkStr = config.getString("bitcoin-s.network")

    BitcoinNetworks.fromString(networkStr).chainParams
  }

  /** The blockchain network we're on */
  lazy val network: BitcoinNetwork = chain.network

  def configFileName: String = AppConfig.DEFAULT_BITCOIN_S_CONF_FILE

  protected lazy val config: Config = {
    val finalConfig =
      AppConfig.getBaseConfig(baseDatadir, configFileName, configOverrides)

    logger.trace(s"Resolved bitcoin-s config:")
    logger.trace(finalConfig.asReadableJson)

    val resolved = {
      ConfigFactory
        .defaultOverrides(getClass.getClassLoader)
        .withFallback(finalConfig)
        .resolve()
    }

    resolved.checkValid(ConfigFactory.defaultReference(), "bitcoin-s")

    resolved
  }

  /** The base data directory. This is where we look for a configuration file */
  protected[bitcoins] def baseDatadir: Path

  /** The network specific data directory. */
  lazy val datadir: Path = {
    val lastDirname = network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }
    baseDatadir.resolve(lastDirname)
  }

  def getConfigStringOpt(path: String): Option[String] = {
    config.getStringOrNone(path)
  }

  def getConfigString(path: String): String = {
    config.getString(path)
  }
}

object AppConfig extends BitcoinSLogger {

  def safePathToString(path: Path): String = {
    val pathStr = path.toString.replace("\\", "/")

    s""""$pathStr"""" // Add quotes around it
  }

  def configToString(config: Config): String = {
    config
      .entrySet()
      .asScala
      .toVector
      .map { entry => s"${entry.getKey} = ${entry.getValue.render()}" }
      .mkString("\n")
  }

  def getBaseConfig(
      baseDatadir: Path,
      configFileName: String,
      configOverrides: Vector[Config]
  ): Config = {
    val configOptions =
      ConfigParseOptions
        .defaults()
        .setClassLoader(getClass().getClassLoader())
    val datadirConfig = {
      val file = baseDatadir.resolve(configFileName)
      val config = if (Files.isReadable(file)) {
        ConfigFactory.parseFile(file.toFile, configOptions)
      } else {
        ConfigFactory.empty()
      }

      val withDatadir =
        ConfigFactory.parseString(
          s"bitcoin-s.datadir = ${safePathToString(baseDatadir)}"
        )
      withDatadir.withFallback(config)
    }

    // we want to NOT resolve substitutions in the configuration until the user
    // provided configs also has been loaded. .parseResources() does not do that
    // whereas .load() does
    val classPathConfig = {
      val applicationConf =
        ConfigFactory.parseResources("application.conf", configOptions)
      val referenceConf =
        ConfigFactory.parseResources("reference.conf", configOptions)
      applicationConf.withFallback(referenceConf)
    }

    // we want the data directory configuration
    // to take preference over any bundled (classpath)
    // configurations
    // loads reference.conf (provided by Bitcoin-S)
    val unresolvedConfig = datadirConfig
      .withFallback(classPathConfig)

    val withOverrides =
      if (configOverrides.nonEmpty) {
        val overrides =
          configOverrides
            // we reverse to make the configs specified last take precedent
            .reverse
            .reduce(_.withFallback(_))

        // to make the overrides actually override
        // the default settings we have to do it
        // in this order
        overrides.withFallback(unresolvedConfig)
      } else {
        unresolvedConfig
      }
    withOverrides
  }

  /** The default data directory
    *
    * TODO: use different directories on Windows and Mac, should probably mimic
    * what Bitcoin Core does
    */
  private[bitcoins] lazy val DEFAULT_BITCOIN_S_DATADIR: Path = {
    val base = Paths.get(Properties.userHome, ".bitcoin-s")
    if (EnvUtil.isLinux) {
      base
    } else if (EnvUtil.isMac) {
      // migration code to use proper location on mac
      val full = Paths
        .get(Properties.userHome)
        .resolve("Library")
        .resolve("Application Support")
        .resolve("bitcoin-s")
      if (Files.exists(full)) {
        full
      } else {
        if (Files.exists(base)) {
          // just use old directory for now
          // we will eventually migrate this in the future
          base
        } else {
          // fresh install, so use the proper spot
          full
        }
      }
    } else if (EnvUtil.isWindows) {
      // windows
      base
    } else {
      sys.error(s"Unsupported os=${EnvUtil.osName}")
    }

  }

  private[bitcoins] lazy val DEFAULT_BITCOIN_S_CONF_FILE: String =
    "bitcoin-s.conf"

  /** Matches the default data directory location with a network appended, both
    * with and without a trailing `/`
    */
  private lazy val defaultDatadirRegex: Regex = {
    // Fix for windows
    val home = Properties.userHome.replace('\\', '/')

    (home + "/.bitcoin-s/(testnet3|mainnet|regtest)/?$").r
  }

  /** Throws if the encountered datadir is the default one. Useful in tests, to
    * make sure you don't blow up important data.
    */
  private[bitcoins] def throwIfDefaultDatadir(config: AppConfig): Unit = {
    val datadirStr = config.datadir.toString()
    defaultDatadirRegex.findFirstMatchIn(datadirStr) match {
      case None => () // pass
      case Some(_) =>
        val errMsg =
          List(
            "It looks like you haven't changed the data directory in your test configuration.",
            s"Your data directory is $datadirStr. This would cause tests to potentially",
            "overwrite your existing data, which you probably don't want."
          ).mkString(" ")
        logger.error(errMsg)
        logger.error(s"Configuration: ${config.config.asReadableJson}")
        throw new RuntimeException(errMsg)
    }
  }
}
