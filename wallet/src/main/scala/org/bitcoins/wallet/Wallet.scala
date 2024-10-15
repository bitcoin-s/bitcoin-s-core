package org.bitcoins.wallet

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.api.wallet.*
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.*
import org.bitcoins.core.gcs.{GolombFilter, SimpleFilterMatcher}
import org.bitcoins.core.hd.*
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.util.{FutureUtil, HDUtil}
import org.bitcoins.core.wallet.fee.*
import org.bitcoins.crypto.*
import org.bitcoins.db.SafeDatabase
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.internal.*
import org.bitcoins.wallet.models.*
import slick.dbio.{DBIOAction, Effect, NoStream}

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

case class Wallet(
    override val nodeApi: NodeApi,
    override val chainQueryApi: ChainQueryApi
)(implicit
    val walletConfig: WalletAppConfig
) extends NeutrinoHDWalletApi
    with WalletLogger {
  def keyManager: BIP39KeyManager = {
    walletConfig.kmConf.toBip39KeyManager
  }
  def feeRateApi: FeeRateApi = walletConfig.feeRateApi
  implicit val system: ActorSystem = walletConfig.system

  implicit val ec: ExecutionContext = system.dispatcher

  private[wallet] lazy val scheduler = walletConfig.scheduler

  val chainParams: ChainParams = walletConfig.chain

  val networkParameters: BitcoinNetwork = walletConfig.network

  private[bitcoins] val walletDAOs: WalletDAOs =
    WalletDAOs.fromWalletConfig(walletConfig)

  private[bitcoins] val addressDAO: AddressDAO = walletDAOs.addressDAO
  private[bitcoins] val accountDAO: AccountDAO = walletDAOs.accountDAO
  private[bitcoins] val spendingInfoDAO: SpendingInfoDAO = walletDAOs.utxoDAO
  private[bitcoins] val transactionDAO: TransactionDAO =
    walletDAOs.transactionDAO
  private[bitcoins] val scriptPubKeyDAO: ScriptPubKeyDAO =
    walletDAOs.scriptPubKeyDAO

  private[bitcoins] val incomingTxDAO: IncomingTransactionDAO =
    walletDAOs.incomingTxDAO

  private[bitcoins] val outgoingTxDAO: OutgoingTransactionDAO =
    walletDAOs.outgoingTxDAO
  private[bitcoins] val addressTagDAO: AddressTagDAO = walletDAOs.addressTagDAO

  private[bitcoins] val stateDescriptorDAO: WalletStateDescriptorDAO =
    walletDAOs.stateDescriptorDAO
  protected lazy val safeDatabase: SafeDatabase = spendingInfoDAO.safeDatabase

  val creationTime: Instant = keyManager.creationTime

  def utxoHandling: UtxoHandling =
    UtxoHandling(spendingInfoDAO, transactionDAO, addressDAO, chainQueryApi)

  def fundTxHandling: FundTransactionHandling = FundTransactionHandling(
    accountHandling = accountHandling,
    utxoHandling = utxoHandling,
    addressHandling = addressHandling,
    transactionProcessing = transactionProcessing,
    spendingInfoDAO = spendingInfoDAO,
    transactionDAO = transactionDAO,
    keyManager = keyManager,
    feeRateApi = feeRateApi
  )
  def accountHandling: AccountHandling =
    AccountHandling(walletDAOs, keyManager)

  def addressHandling: AddressHandling =
    AddressHandling(accountHandling, walletDAOs)

  override def transactionProcessing: TransactionProcessingApi = {
    TransactionProcessing(
      walletApi = this,
      chainQueryApi = chainQueryApi,
      utxoHandling = utxoHandling,
      walletDAOs = walletDAOs
    )
  }
  override def rescanHandling: RescanHandlingApi = {
    RescanHandling(
      transactionProcessing = transactionProcessing,
      accountHandling = accountHandling,
      addressHandling = addressHandling,
      chainQueryApi = chainQueryApi,
      nodeApi = nodeApi,
      walletDAOs = walletDAOs
    )
  }

  override def sendFundsHandling: SendFundsHandlingApi = {
    SendFundsHandlingHandling(
      accountHandling = accountHandling,
      feeRateApi = feeRateApi,
      fundTxHandling = fundTxHandling,
      addressHandling = addressHandling,
      transactionProcessing = transactionProcessing,
      utxoHandling = utxoHandling,
      keyManager = keyManager,
      walletDAOs = walletDAOs
    )
  }

  override def isRescanning(): Future[Boolean] = rescanHandling.isRescanning()

  def walletCallbacks: WalletCallbacks = walletConfig.callBacks

  override def getNewAddress(): Future[BitcoinAddress] = {
    addressHandling.getNewAddress()
  }

  override def getNewChangeAddress(): Future[BitcoinAddress] = {
    addressHandling.getNewChangeAddress()
  }

  override def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] = {
    stateDescriptorDAO.getSyncHeight()
  }

  override def getSyncState(): Future[BlockSyncState] = {
    getSyncDescriptorOpt().map {
      case Some(descriptor) =>
        BlockSyncState(descriptor.height, descriptor.bestHash)
      case None =>
        BlockSyncState(0, chainParams.genesisHashBE)
    }
  }

  override def processCompactFilters(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)]
  ): Future[Wallet] = {
    val utxosF = utxoHandling.getUtxos()
    val spksF = addressHandling.getScriptPubKeys()
    val blockHashOpt = blockFilters.lastOption.map(_._1)
    val heightOptF = blockHashOpt match {
      case Some(blockHash) =>
        chainQueryApi.getBlockHeight(blockHash)
      case None => Future.successful(None)
    }
    for {
      utxos <- utxosF
      scripts <- spksF
      _ = logger.debug(
        s"Processing ${blockFilters.length} block filters for ${utxos.length} utxos and ${scripts.length} scripts"
      )
      scriptPubKeys =
        utxos.flatMap(_.redeemScriptOpt).toSet ++ scripts.map(_.scriptPubKey)
      blockHashToDownload <- {
        if (scriptPubKeys.isEmpty) {
          // do nothing as an optimization, if we have nothing in the wallet
          // we don't need to search the filters
          Future.successful(Vector.empty)
        } else {
          FutureUtil
            .batchAndParallelExecute(
              elements = blockFilters,
              f = searchFilterMatches(scriptPubKeys.toVector)
            )
            .map(_.flatten)
        }
      }
      _ <- nodeApi.downloadBlocks(blockHashToDownload)
      hash = blockFilters.last._1
      heightOpt <- heightOptF
      _ <- {
        heightOpt match {
          case Some(height) =>
            if (blockHashToDownload.isEmpty) {
              // if we don't have any block hashes
              // we need to update the wallet's sync height
              stateDescriptorDAO
                .updateSyncHeight(hash, height)
                .map(_ => ())
            } else {
              // if we do have a block hash that we matched
              // we need to let wallet.processBlock()
              // update the wallet's sync height
              Future.unit
            }
          case None =>
            Future.unit
        }
      }
    } yield {
      this
    }
  }

  private def searchFilterMatches(spks: Vector[ScriptPubKey])(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)]
  ): Future[Vector[DoubleSha256DigestBE]] = FutureUtil.makeAsync { () =>
    val asmVec = spks.map(_.asmBytes)
    blockFilters.flatMap { case (blockHash, blockFilter) =>
      val matcher = SimpleFilterMatcher(blockFilter)
      if (matcher.matchesAny(asmVec)) {
        Vector(blockHash)
      } else {
        Vector.empty
      }
    }
  }

  override def broadcastTransaction(transaction: Transaction): Future[Unit] =
    for {
      _ <- nodeApi.broadcastTransaction(transaction)
      _ <- transactionProcessing.processTransaction(transaction,
                                                    blockHashWithConfsOpt =
                                                      None)
      _ <- walletCallbacks.executeOnTransactionBroadcast(transaction)
    } yield ()

  override def isEmpty(): Future[Boolean] =
    for {
      addressCount <- addressDAO.count()
      spendingInfoCount <- spendingInfoDAO.count()
    } yield addressCount == 0 && spendingInfoCount == 0

  override def getBalance()(implicit
      ec: ExecutionContext
  ): Future[CurrencyUnit] = {
    utxoHandling.getBalance()
  }

  override def getConfirmedBalance(): Future[CurrencyUnit] = {
    utxoHandling.getConfirmedBalance()
  }

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = {
    utxoHandling.getUnconfirmedBalance()
  }

  override def getWalletName(): Future[String] = {
    Future.successful(walletConfig.walletName)
  }

  override def getInfo(): Future[WalletInfo] = {
    for {
      accountDb <- accountHandling.getDefaultAccount()
      walletState <- getSyncState()
      rescan <- rescanHandling.isRescanning()
    } yield {
      WalletInfo(
        walletName = walletConfig.walletName,
        rootXpub = keyManager.getRootXPub,
        xpub = accountDb.xpub,
        hdAccount = accountDb.hdAccount,
        height = walletState.height,
        blockHash = walletState.blockHash,
        rescan = rescan,
        imported = keyManager.imported
      )
    }
  }

  def startFeeRateCallbackScheduler(): Unit = {
    val feeRateChangedRunnable = new Runnable {
      override def run(): Unit = {
        getFeeRate()
          .map(feeRate => Some(feeRate))
          .recover { case NonFatal(_) =>
            // logger.error("Cannot get fee rate ", ex)
            None
          }
          .foreach { feeRateOpt =>
            walletCallbacks.executeOnFeeRateChanged(
              feeRateOpt.getOrElse(SatoshisPerVirtualByte.negativeOne)
            )
          }
        ()
      }
    }

    val _ = scheduler.scheduleAtFixedRate(
      feeRateChangedRunnable,
      walletConfig.feeRatePollDelay.toSeconds,
      walletConfig.feeRatePollInterval.toSeconds,
      TimeUnit.SECONDS
    )
  }
}

object Wallet extends WalletLogger {

  /** Creates the master xpub for the key manager in the database
    * @throws RuntimeException
    *   if a different master xpub key exists in the database
    */
  private def createMasterXPub(
      keyManager: BIP39KeyManager
  )(implicit walletAppConfig: WalletAppConfig): Future[ExtPublicKey] = {
    import walletAppConfig.ec
    val masterXPubDAO = MasterXPubDAO()
    val countF = masterXPubDAO.count()
    // make sure we don't have a xpub in the db
    countF.flatMap { count =>
      if (count == 0) {
        masterXPubDAO.create(keyManager.getRootXPub).map(_.toExtPublicKey)
      } else {
        for {
          xpubs <- masterXPubDAO.findAll()
        } yield {
          if (
            xpubs.length == 1 && xpubs.head.toExtPublicKey == keyManager.getRootXPub
          ) {
            xpubs.head.toExtPublicKey
          } else {
            throw new IllegalArgumentException(
              s"Wallet database contains different master xpubs, got=${xpubs}"
            )
          }
        }
      }
    }

  }

  /** Creates the level 0 account for the given HD purpose, if the root account
    * exists do nothing
    */
  private def createRootAccount(wallet: Wallet, keyManager: BIP39KeyManager)(
      implicit ec: ExecutionContext
  ): DBIOAction[AccountDb, NoStream, Effect.Read & Effect.Write] = {
    val coinType = HDUtil.getCoinType(keyManager.kmParams.network)
    val coin =
      HDCoin(purpose = keyManager.kmParams.purpose, coinType = coinType)
    val account = HDAccount(coin = coin, index = 0)
    // safe since we're deriving from a priv
    val xpub = keyManager.deriveXPub(account).get
    val accountDb = AccountDb(xpub, account)

    // see if we already have this account in our database
    // Three possible cases:
    // 1. We have nothing in our database, so we need to insert it
    // 2. We already have this account in our database, so we do nothing
    // 3. We have this account in our database, with a DIFFERENT xpub. This is bad. Fail with an exception
    //   this most likely means that we have a different key manager than we expected
    wallet.accountDAO
      .findByPrimaryKeyAction((account.coin, account.index))
      .flatMap {
        case Some(account) =>
          if (account.xpub != xpub) {
            val errorMsg =
              s"Divergent xpubs for account=${account}. Existing database xpub=${account.xpub}, new xpub=${xpub}. " +
                s"It is possible we have a different key manager being used than expected, keymanager=${keyManager.kmParams.seedPath.toAbsolutePath.toString}"
            DBIOAction.failed(new RuntimeException(errorMsg))
          } else {
            logger.debug(
              s"Account already exists in database, no need to create it, account=${account}"
            )
            DBIOAction.successful(account)
          }
        case None =>
          wallet.accountDAO
            .createAction(accountDb)
      }
  }

  def initialize(
      wallet: Wallet,
      accountHandling: AccountHandlingApi,
      bip39PasswordOpt: Option[String]
  ): Future[Wallet] = {
    implicit val walletAppConfig: WalletAppConfig = wallet.walletConfig
    import walletAppConfig.ec
    val passwordOpt = walletAppConfig.aesPasswordOpt

    val createMasterXpubF = createMasterXPub(wallet.keyManager)
    // We want to make sure all level 0 accounts are created,
    // so the user can change the default account kind later
    // and still have their wallet work
    val createAccountActions: Vector[
      DBIOAction[AccountDb, NoStream, Effect.Read & Effect.Write]
    ] = {
      val accounts = HDPurpose.singleSigPurposes.map { purpose =>
        // we need to create key manager params for each purpose
        // and then initialize a key manager to derive the correct xpub
        val kmParams = wallet.keyManager.kmParams.copy(purpose = purpose)
        val kmE = {
          BIP39KeyManager.fromParams(
            kmParams = kmParams,
            passwordOpt = passwordOpt,
            bip39PasswordOpt = bip39PasswordOpt
          )
        }
        kmE match {
          case Right(km) =>
            createRootAccount(wallet = wallet, keyManager = km)
          case Left(err) =>
            // probably means you haven't initialized the key manager via the
            // 'CreateKeyManagerApi'
            DBIOAction.failed(
              new RuntimeException(
                s"Failed to create keymanager with params=$kmParams err=$err"
              )
            )
        }

      }
      accounts
    }
    for {
      _ <- createMasterXpubF
      actions = createAccountActions
      accounts <- wallet.accountDAO.safeDatabase.runVec(
        DBIOAction.sequence(actions)
      )
      _ = accounts.foreach { a =>
        logger.info(s"Created account=${a} to DB")
      }
      _ <- {
        // check if creationTime is well in the past, if so generate a pool of addresses
        // see: https://github.com/bitcoin-s/bitcoin-s/issues/5033
        val creationTime = wallet.keyManager.creationTime
        val threshold = Instant.now().minus(1, ChronoUnit.HOURS)
        val isOldCreationTime = creationTime.compareTo(threshold) <= 0
        if (isOldCreationTime) {
          accountHandling
            .generateScriptPubKeys(
              account = walletAppConfig.defaultAccount,
              addressBatchSize = walletAppConfig.discoveryBatchSize,
              forceGenerateSpks = true
            )
            .map(_ => ())
        } else {
          // fresh seed, no need to generate addresses
          Future.unit
        }
      }
    } yield {
      logger.debug(s"Created root level accounts for wallet")
      wallet
    }
  }
}
