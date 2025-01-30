package org.bitcoins.dlc.oracle.config

import com.typesafe.config.Config
import org.bitcoins.commons.config.AppConfigFactory
import org.bitcoins.core.api.dlcoracle.db.EventOutcomeDbHelper
import org.bitcoins.core.config._
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.protocol.blockchain.{
  BitcoinChainParams,
  TestNetChainParams
}
import org.bitcoins.core.protocol.tlv.EnumEventDescriptorV0TLV
import org.bitcoins.core.wallet.keymanagement.KeyManagerParams
import org.bitcoins.crypto.AesPassword
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.db._
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.db.util.{DBMasterXPubApi, MasterXPubUtil}
import org.bitcoins.dlc.oracle.DLCOracle
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.keymanager.config.KeyManagerAppConfig

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}

case class DLCOracleAppConfig(
    baseDatadir: Path,
    configOverrides: Vector[Config]
)(implicit val ec: ExecutionContext)
    extends DbAppConfig
    with DbManagement
    with JdbcProfileComponent[DLCOracleAppConfig]
    with DBMasterXPubApi {

  import profile.api._

  override def appConfig: DLCOracleAppConfig = this

  override type ConfigType = DLCOracleAppConfig

  override def newConfigOfType(configs: Vector[Config]): DLCOracleAppConfig =
    DLCOracleAppConfig(baseDatadir, configs)

  /** DLC oracles are not network specific, so just hard code the testnet chain
    * params
    */
  final override lazy val chain: BitcoinChainParams = TestNetChainParams

  /** DLC oracles are not network specific, so just hard code the network */
  final override lazy val network: BitcoinNetwork = chain.network

  override def moduleName: String = DLCOracleAppConfig.moduleName

  lazy val kmConf: KeyManagerAppConfig =
    KeyManagerAppConfig(baseDatadir, configOverrides)

  /** The path to our encrypted mnemonic seed */
  lazy val seedPath: Path = kmConf.seedPath

  override lazy val datadir: Path = {
    baseDatadir.resolve("oracle")
  }

  override def start(): Future[Unit] = {
    logger.debug(s"Initializing dlc oracle setup")
    val migrationsF = for {
      _ <- super.start()
      _ <- kmConf.start()
      numMigrations = migrate()
      _ = logger.info(s"Applied $numMigrations to the dlc oracle project")
    } yield {
      if (Files.notExists(datadir)) {
        Files.createDirectories(datadir)
      }
      val networkDir = {
        val lastDirname = network match {
          case MainNet  => "mainnet"
          case TestNet3 => "testnet3"
          case RegTest  => "regtest"
          case SigNet   => "signet"
        }
        baseDatadir.resolve(lastDirname)
      }
      // Move old db in network folder to oracle folder
      val oldNetworkLocation = networkDir.resolve("oracle.sqlite")
      if (!Files.exists(dbPath) && Files.exists(oldNetworkLocation)) {
        Files.move(oldNetworkLocation, dbPath)
      }

      val migrations = migrationsApplied()
      migrations
    }

    migrationsF.flatMap { migrations =>
      val migrationWorkAroundF = v2V3MigrationWorkaround(migrations)

      val initializeF = initializeKeyManager()
      for {
        _ <- initializeF
        oracle = new DLCOracle()(this)
        _ <- MasterXPubUtil.checkMasterXPub(oracle.getRootXpub, masterXPubDAO)
        _ <- migrationWorkAroundF
      } yield {
        if (isHikariLoggingEnabled) {
          // .get is safe because hikari logging is enabled
          startHikariLogger(hikariLoggingInterval.get)
          ()
        } else {
          ()
        }
      }
    }
  }

  def rpcPort: Int = config.getInt("bitcoin-s.oracle.rpcport")

  def rpcBindOpt: Option[String] = {
    if (config.hasPath("bitcoin-s.oracle.rpcbind")) {
      Some(config.getString("bitcoin-s.oracle.rpcbind"))
    } else {
      None
    }
  }

  def rpcPassword: String = config.getString("bitcoin-s.oracle.password")

  lazy val kmParams: KeyManagerParams =
    KeyManagerParams(
      kmConf.seedPath,
      HDPurpose(DLCOracle.R_VALUE_PURPOSE),
      network
    )

  lazy val aesPasswordOpt: Option[AesPassword] = kmConf.aesPasswordOpt
  lazy val bip39PasswordOpt: Option[String] = kmConf.bip39PasswordOpt

  def exists(): Future[Boolean] = {
    lazy val hasDb = this.driver match {
      case PostgreSQL => true
      case SQLite     => Files.exists(dbPath)
    }
    seedExists().map(bool => bool && hasDb)
  }

  /** Insert the master xpub for the oracle */
  private def initializeKeyManager(): Future[Unit] = {
    val initF = seedExists().flatMap { bool =>
      if (!bool) {
        BIP39KeyManager.fromParams(
          kmParams = kmParams,
          passwordOpt = aesPasswordOpt,
          bip39PasswordOpt = bip39PasswordOpt
        ) match {
          case Left(err) => sys.error(err.toString)
          case Right(km) =>
            logger.info("Successfully generated a seed and key manager")
            masterXPubDAO
              .create(km.getRootXPub)
              .map(_ => ())
        }
      } else {
        logger.info(s"Not initializing key manager, seed already exists")
        Future.unit
      }
    }
    initF
  }

  private val masterXPubDAO: MasterXPubDAO = MasterXPubDAO()(ec, this)

  private lazy val masterXPubTable: TableQuery[Table[?]] = {
    masterXPubDAO.table
  }

  private lazy val rValueTable: TableQuery[Table[?]] = {
    RValueDAO()(ec, appConfig).table
  }

  private lazy val eventTable: TableQuery[Table[?]] = {
    EventDAO()(ec, appConfig).table
  }

  private lazy val eventOutcomeTable: TableQuery[Table[?]] = {
    EventOutcomeDAO()(ec, appConfig).table
  }

  override def allTables: List[TableQuery[Table[?]]] =
    List(masterXPubTable, rValueTable, eventTable, eventOutcomeTable)

  /** @param migrations - The number of migrations we have run */
  private def v2V3MigrationWorkaround(migrations: Int): Future[Unit] = {
    val migrationWorkAroundF: Future[Unit] = {
      if (migrations == 2 || migrations == 3) { // For V2/V3 migrations
        logger.debug(s"Doing V2/V3 Migration")

        val dummyMigrationTLV = EnumEventDescriptorV0TLV.dummy

        val eventDAO = EventDAO()(ec, appConfig)
        for {
          // get all old events
          allEvents <- eventDAO.findByEventDescriptor(dummyMigrationTLV)
          allOutcomes <- EventOutcomeDAO()(ec, appConfig).findAll()

          outcomesByNonce = allOutcomes.groupBy(_.nonce)
          // Update them to have the correct event descriptor
          updated = allEvents.map { eventDb =>
            val outcomeDbs = outcomesByNonce(eventDb.nonce)
            val descriptor =
              EventOutcomeDbHelper.createEnumEventDescriptor(outcomeDbs)
            eventDb.copy(eventDescriptorTLV = descriptor)
          }

          _ <- eventDAO.upsertAll(updated)
        } yield ()
      } else {
        Future.unit
      }
    }
    migrationWorkAroundF
  }
}

object DLCOracleAppConfig extends AppConfigFactory[DLCOracleAppConfig] {

  override val moduleName: String = "oracle"

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext
  ): DLCOracleAppConfig =
    DLCOracleAppConfig(datadir, confs)
}
