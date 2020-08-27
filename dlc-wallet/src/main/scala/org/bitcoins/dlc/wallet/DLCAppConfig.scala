package org.bitcoins.dlc.wallet

import java.nio.file.{Files, Path}

import com.typesafe.config.Config
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.keymanagement.KeyManagerInitializeError
import org.bitcoins.db.{AppConfig, AppConfigFactory, JdbcProfileComponent}
import org.bitcoins.keymanager.bip39.{BIP39KeyManager, BIP39LockedKeyManager}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  *
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class DLCAppConfig(private val directory: Path, private val conf: Config*)(
    implicit override val ec: ExecutionContext)
    extends AppConfig
    with DLCDbManagement
    with JdbcProfileComponent[DLCAppConfig] {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList
  override protected[bitcoins] def moduleName: String = "dlc"
  override protected[bitcoins] type ConfigType = DLCAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): DLCAppConfig =
    DLCAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def appConfig: DLCAppConfig = this

  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing dlc setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    val numMigrations = {
      migrate()
    }

    logger.info(s"Applied $numMigrations to the dlc project")

    FutureUtil.unit
  }

  /** Starts the associated application */
  override def start(): Future[Unit] = FutureUtil.unit

  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi,
      bip39PasswordOpt: Option[String])(implicit
      walletConf: WalletAppConfig,
      ec: ExecutionContext): Future[DLCWallet] = {
    DLCAppConfig.createDLCWallet(
      nodeApi = nodeApi,
      chainQueryApi = chainQueryApi,
      feeRateApi = feeRateApi,
      bip39PasswordOpt = bip39PasswordOpt)(walletConf, this, ec)
  }
}

object DLCAppConfig extends AppConfigFactory[DLCAppConfig] with WalletLogger {

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): DLCAppConfig =
    DLCAppConfig(datadir, confs: _*)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi,
      bip39PasswordOpt: Option[String])(implicit
      walletConf: WalletAppConfig,
      dlcConf: DLCAppConfig,
      ec: ExecutionContext): Future[DLCWallet] = {
    walletConf.hasWallet().flatMap { walletExists =>
      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        // TODO change me when we implement proper password handling
        BIP39LockedKeyManager.unlock(BIP39KeyManager.badPassphrase,
                                     bip39PasswordOpt,
                                     walletConf.kmParams) match {
          case Right(km) =>
            val wallet =
              DLCWallet(km, nodeApi, chainQueryApi, feeRateApi, km.creationTime)
            Future.successful(wallet)
          case Left(err) =>
            sys.error(s"Error initializing key manager, err=${err}")
        }
      } else {
        logger.info(s"Initializing key manager")
        val bip39PasswordOpt = None
        val keyManagerE: Either[KeyManagerInitializeError, BIP39KeyManager] =
          BIP39KeyManager.initialize(kmParams = walletConf.kmParams,
                                     bip39PasswordOpt = bip39PasswordOpt)

        val keyManager = keyManagerE match {
          case Right(keyManager) => keyManager
          case Left(err) =>
            sys.error(s"Error initializing key manager, err=${err}")
        }

        logger.info(s"Creating new wallet")
        val unInitializedWallet =
          DLCWallet(keyManager,
                    nodeApi,
                    chainQueryApi,
                    feeRateApi,
                    keyManager.creationTime)

        Wallet
          .initialize(wallet = unInitializedWallet,
                      bip39PasswordOpt = bip39PasswordOpt)
          .map(_.asInstanceOf[DLCWallet])
      }
    }
  }
}
