package org.bitcoins.testkit.wallet

import org.apache.pekko.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.WalletApi
import org.bitcoins.core.currency.*
import org.bitcoins.dlc.wallet.{DLCAppConfig, DLCWallet}
import org.bitcoins.node.NodeCallbacks
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.server.{BitcoinSAppConfig, BitcoindRpcBackendUtil}
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.node.MockNodeApi
import org.bitcoins.testkit.wallet.FundWalletUtil.{
  FundedDLCWallet,
  FundedWallet
}
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.*
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletLogger}
import org.scalatest.*

import java.util.UUID
import scala.concurrent.*
import scala.concurrent.duration.*

trait BitcoinSWalletTest
    extends BitcoinSFixture
    with BaseWalletTest
    with EmbeddedPg {
  import BitcoinSWalletTest._

  implicit protected def getFreshDLCAppConfig: DLCAppConfig = {
    getFreshConfig.dlcConf
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.walletConf)
    AppConfig.throwIfDefaultDatadir(getFreshConfig.dlcConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    Await.result(getFreshConfig.chainConf.stop(), 1.minute)
    Await.result(getFreshConfig.nodeConf.stop(), 1.minute)
    Await.result(getFreshConfig.walletConf.stop(), 1.minute)
    Await.result(getFreshConfig.dlcConf.stop(), 1.minute)
    super[EmbeddedPg].afterAll()
    super[BitcoinSFixture].afterAll()
  }

  def nodeApi: NodeApi = MockNodeApi

  /** Lets you customize the parameters for the created wallet */
  val withNewConfiguredWallet: Config => OneArgAsyncTest => FutureOutcome = {
    walletConfig =>
      val bip39PasswordOpt = KeyManagerTestUtil.bip39PasswordOpt
      val bip39PasswordConfig: Config =
        BitcoinSWalletTest.buildBip39PasswordConfig(bip39PasswordOpt)

      val mergedConfig = bip39PasswordConfig.withFallback(walletConfig)
      implicit val newWalletConf =
        getFreshWalletAppConfig.withOverrides(mergedConfig)

      makeDependentFixture[Wallet](
        build =
          createNewWallet(nodeApi = nodeApi, chainQueryApi = chainQueryApi),
        destroy = { (_: WalletApi) =>
          for {
            _ <- destroyWalletAppConfig(newWalletConf)
          } yield ()
        }
      )
  }

  /** Creates a wallet that is funded with some bitcoin, this wallet is NOT
    * peered with a bitcoind so the funds in the wallet are not tied to an
    * underlying blockchain
    */
  def withFundedWallet(
      test: OneArgAsyncTest
  )(implicit walletAppConfig: WalletAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = () => FundWalletUtil.createFundedWallet(nodeApi, chainQueryApi),
      destroy = { (_: FundedWallet) =>
        for {
          _ <- destroyWalletAppConfig(walletAppConfig)
        } yield ()
      }
    )(test)
  }

  def withFundedSegwitWallet(
      test: OneArgAsyncTest
  )(implicit walletAppConfig: WalletAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = () => FundWalletUtil.createFundedWallet(nodeApi, chainQueryApi),
      destroy = { (_: FundedWallet) =>
        for {
          _ <- destroyWalletAppConfig(walletAppConfig)
        } yield ()
      }
    )(test)
  }

  /** Creates a wallet that is funded with some bitcoin, this wallet is NOT
    * peered with a bitcoind so the funds in the wallet are not tied to an
    * underlying blockchain
    */
  def withFundedDLCWallet(
      test: OneArgAsyncTest
  )(implicit config: BitcoinSAppConfig): FutureOutcome = {
    makeDependentFixture(
      build =
        () => FundWalletUtil.createFundedDLCWallet(nodeApi, chainQueryApi),
      destroy = { (funded: FundedDLCWallet) =>
        for {
          _ <- destroyDLCWallet(funded.wallet)
        } yield ()
      }
    )(test)
  }

  /** Fixture for an initialized wallet which produce legacy addresses */
  def withLegacyWallet(test: OneArgAsyncTest): FutureOutcome = {
    withNewConfiguredWallet(BaseWalletTest.legacyWalletConf)(test)
  }

  /** Fixture for an initialized wallet which produce segwit addresses */
  def withSegwitWallet(test: OneArgAsyncTest): FutureOutcome = {
    withNewConfiguredWallet(BaseWalletTest.segwitWalletConf)(test)
  }

  /** Fixture for a wallet with default configuration with no funds in it */
  def withNewWallet(
      test: OneArgAsyncTest
  )(implicit walletAppConfig: WalletAppConfig): FutureOutcome = {
    makeDependentFixture[Wallet](
      build = { () =>
        createDefaultWallet(nodeApi, chainQueryApi)
      },
      destroy = { (_: WalletApi) =>
        for {
          _ <- destroyWalletAppConfig(walletAppConfig)
        } yield ()
      }
    )(test)
  }

  def withNewWallet2Accounts(
      test: OneArgAsyncTest
  )(implicit walletAppConfig: WalletAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = { () =>
        createWallet2Accounts(nodeApi, chainQueryApi)
      },
      destroy = { (_: WalletApi) => Future.unit }
    )(test)
  }

  def withNewWalletAndBitcoind(
      test: OneArgAsyncTest
  )(implicit walletAppConfig: WalletAppConfig): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoindRpc] =
      BitcoinSFixture.composeBuildersAndWrap(
        builder = { () =>
          BitcoinSFixture.createBitcoindWithFunds(Some(BitcoindVersion.newest))
        },
        dependentBuilder = { (bitcoind: BitcoindRpcClient) =>
          createWalletWithBitcoind(bitcoind)
        },
        wrap =
          (_: BitcoindRpcClient, walletWithBitcoind: WalletWithBitcoindRpc) =>
            walletWithBitcoind
      )

    makeDependentFixture(
      builder,
      destroy = destroyWalletWithBitcoind(_: WalletWithBitcoindRpc)
    )(test)
  }

  def withWalletConfig(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletAppConfig] = () => {
      createWalletAppConfig(() => pgUrl(), Vector.empty)
    }

    val destroy: WalletAppConfig => Future[Unit] = walletAppConfig => {
      destroyWalletAppConfig(walletAppConfig)
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }

  def withWalletConfigNotStarted(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletAppConfig] = () => {
      createWalletAppConfigNotStarted(() => pgUrl(), Vector.empty)
    }

    val destroy: WalletAppConfig => Future[Unit] = _ => {
      // it might not be started, so don't stop it
      Future.unit
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }
}

object BitcoinSWalletTest extends WalletLogger {

  val defaultAcctAmts = Vector(1.bitcoin, 2.bitcoin, 3.bitcoin)

  val expectedDefaultAmt: CurrencyUnit =
    defaultAcctAmts.fold(CurrencyUnits.zero)(_ + _)

  val account1Amt = Vector(Bitcoins(0.2), Bitcoins(0.3), Bitcoins(0.5))

  val expectedAccount1Amt: CurrencyUnit =
    account1Amt.fold(CurrencyUnits.zero)(_ + _)

  lazy val initialFunds: CurrencyUnit = expectedDefaultAmt + expectedAccount1Amt

  def createWalletAppConfig(
      pgUrl: () => Option[String],
      configs: Vector[Config]
  )(implicit system: ActorSystem): Future[WalletAppConfig] = {
    import system.dispatcher
    val walletAppConfigF = createWalletAppConfigNotStarted(pgUrl, configs)
    for {
      appConfig <- walletAppConfigF
      _ <- appConfig.start()
    } yield appConfig
  }

  def createWalletAppConfigNotStarted(
      pgUrl: () => Option[String],
      configs: Vector[Config]
  )(implicit system: ActorSystem): Future[WalletAppConfig] = {
    val baseConf = BaseWalletTest.getFreshWalletAppConfig(pgUrl, configs)
    val walletNameOpt = if (NumberGenerator.bool.sampleSome) {
      Some(UUID.randomUUID().toString.replace("-", ""))
    } else None

    val walletConf = walletNameOpt match {
      case Some(walletName) =>
        val walletNameOverride = ConfigFactory.parseString(
          s"bitcoin-s.wallet.walletName = $walletName"
        )

        BitcoinSAppConfig(
          baseConf.baseDatadir,
          (walletNameOverride +: baseConf.configOverrides)
        ).walletConf
      case None => baseConf
    }

    Future.successful(walletConf)
  }

  /** Returns a function that can be used to create a wallet fixture. If you
    * pass in a configuration to this method that configuration is given to the
    * wallet as user-provided overrides. You could for example use this to
    * override the default data directory, network or account type.
    */
  private def createNewWallet(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit walletConfig: WalletAppConfig
  ): () => Future[Wallet] = { () =>
    {
      import walletConfig.ec
      // we want to check we're not overwriting
      // any user data
      AppConfig.throwIfDefaultDatadir(walletConfig)

      walletConfig.start().flatMap { _ =>
        val wallet =
          Wallet(nodeApi, chainQueryApi)(walletConfig)
        Wallet.initialize(wallet,
                          wallet.accountHandling,
                          walletConfig.bip39PasswordOpt)
      }
    }
  }

  private def createDLCWallet(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit
      config: BitcoinSAppConfig,
      ec: ExecutionContext
  ): Future[DLCWallet] = {

    // we want to check we're not overwriting
    // any user data
    AppConfig.throwIfDefaultDatadir(config.walletConf)

    val initConfs = for {
      _ <- config.walletConf.start()
      _ <- config.dlcConf.start()
    } yield ()

    initConfs.flatMap { _ =>
      val wallet = Wallet(nodeApi, chainQueryApi)(config.walletConf)

      Wallet
        .initialize(wallet,
                    wallet.accountHandling,
                    config.walletConf.bip39PasswordOpt)
        .map(w =>
          DLCWallet(w)(
            config.dlcConf,
            config.walletConf
          ))
    }
  }

  /** Creates a wallet with the default configuration */
  def createDefaultWallet(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit walletAppConfig: WalletAppConfig
  ): Future[Wallet] = {
    createNewWallet(nodeApi = nodeApi, chainQueryApi = chainQueryApi)(
      walletAppConfig
    )() // get the standard config
  }

  /** Creates a default wallet with bitcoind where the [[ChainQueryApi]] fed to
    * the wallet is implemented by bitcoind
    */
  def createWalletWithBitcoindCallbacks(bitcoind: BitcoindRpcClient)(implicit
      walletAppConfig: WalletAppConfig,
      system: ActorSystem
  ): Future[WalletWithBitcoindRpc] = {
    import system.dispatcher
    val walletWithBitcoindF = for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(bitcoind, bitcoind)
      // create the wallet with the appropriate callbacks now that
      // we have them
      walletWithCallback = Wallet(
        nodeApi = BitcoindRpcBackendUtil.buildBitcoindNodeApi(
          bitcoind,
          wallet,
          None
        ),
        chainQueryApi = bitcoind
      )(wallet.walletConfig)
    } yield WalletWithBitcoindRpc(
      walletWithCallback,
      bitcoind,
      walletWithCallback.walletConfig
    )

    walletWithBitcoindF
  }

  def createWallet2Accounts(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi
  )(implicit config: WalletAppConfig, system: ActorSystem): Future[Wallet] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val defaultWalletF =
      createDefaultWallet(nodeApi, chainQueryApi)
    for {
      wallet <- defaultWalletF
      account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
      _ <- wallet.accountHandling.createNewAccount(
        hdAccount = account1
      )
    } yield wallet

  }

  def createDLCWallet2Accounts(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit
      config: BitcoinSAppConfig,
      system: ActorSystem
  ): Future[DLCWallet] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    for {
      wallet <- createDLCWallet(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi
      )
      account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
      _ <- wallet.accountHandling.createNewAccount(
        hdAccount = account1
      )
    } yield wallet
  }

  /** Pairs the given wallet with a bitcoind instance that has money in the
    * bitcoind wallet
    */
  def createWalletWithBitcoind(
      wallet: Wallet
  )(implicit system: ActorSystem): Future[WalletWithBitcoindRpc] = {
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds()
    bitcoindF.map(bitcoind =>
      WalletWithBitcoindRpc(wallet, bitcoind, wallet.walletConfig))(
      system.dispatcher)
  }

  /** Pairs the given wallet with a bitcoind instance that has money in the
    * bitcoind wallet
    */
  def createWalletWithBitcoind(
      wallet: Wallet,
      versionOpt: Option[BitcoindVersion]
  )(implicit system: ActorSystem): Future[WalletWithBitcoindRpc] = {
    import system.dispatcher
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds(versionOpt)
    bitcoindF.map(bitcoind =>
      WalletWithBitcoindRpc(wallet, bitcoind, wallet.walletConfig))
  }

  def createWalletWithBitcoind(bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem,
      config: WalletAppConfig
  ): Future[WalletWithBitcoindRpc] = {
    createWalletWithBitcoindCallbacks(bitcoind)
  }

  def createWalletWithBitcoind(
      wallet: Wallet,
      bitcoindRpcClient: BitcoindRpcClient
  ): Future[WalletWithBitcoindRpc] = {
    Future.successful(
      WalletWithBitcoindRpc(wallet, bitcoindRpcClient, wallet.walletConfig)
    )
  }

  /** Gives us a funded bitcoin-s wallet and the bitcoind instance that funded
    * that wallet
    */
  def fundedWalletAndBitcoind(
      versionOpt: Option[BitcoindVersion],
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      walletCallbacks: WalletCallbacks
  )(implicit
      config: BitcoinSAppConfig,
      system: ActorSystem
  ): Future[WalletWithBitcoindRpc] = {
    import system.dispatcher
    config.walletConf.addCallbacks(walletCallbacks)
    for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(
        nodeApi,
        chainQueryApi
      )(config.walletConf, system)
      withBitcoind <- createWalletWithBitcoind(wallet, versionOpt)
      funded <- FundWalletUtil.fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  /** Funds a wallet with bitcoind, this method adds
    * [[CallbackUtil.createNeutrinoNodeCallbacksForWallet()]] which processes
    * filters/blocks that can be used to fund the wallet.
    *
    * It's important to note that this does NOT synchronize the wallet with a
    * chain state. This should be done by the caller of this method. A useful
    * method to help you with that in neutrino node cases is
    * [[BitcoinSWalletTest.awaitWalletBalances]]
    */
  def fundedWalletAndBitcoind(
      bitcoindRpcClient: BitcoindRpcClient,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      walletCallbacks: WalletCallbacks
  )(implicit
      config: BitcoinSAppConfig,
      system: ActorSystem
  ): Future[WalletWithBitcoindRpc] = {
    import system.dispatcher
    config.walletConf.addCallbacks(walletCallbacks)
    for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi
      )(config.walletConf, system)
      // add callbacks for wallet
      nodeCallbacks <-
        BitcoinSWalletTest.createNeutrinoNodeCallbacksForWallet(wallet)(system)
      _ = config.nodeConf.addCallbacks(nodeCallbacks)
      withBitcoind <- createWalletWithBitcoind(wallet, bitcoindRpcClient)
      funded <- FundWalletUtil.fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  def destroyWalletWithBitcoind[T <: BitcoindRpcClient](
      walletWithBitcoind: WalletWithBitcoind[T]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    val bitcoind = walletWithBitcoind.bitcoind
    val stopF = bitcoind.stop()
    val destroyWalletF = destroyOnlyWalletWithBitcoindCached(walletWithBitcoind)
    for {
      _ <- stopF
      _ <- destroyWalletF
    } yield ()
  }

  /** Destorys the [[WalletApi]] and [[WalletAppConfig]] inside of
    * [[WalletWithBitcoind]]. This method does not touch the bitcoind instance
    * so it can be re-used in [[CachedBitcoind]] tests
    */
  def destroyOnlyWalletWithBitcoindCached(
      walletWithBitcoind: WalletWithBitcoind[?]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- destroyWalletAppConfig(walletWithBitcoind.walletConfig)
    } yield ()
  }

  def destroyDLCWallet(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- destroyWalletAppConfig(wallet.walletConfig)
      _ <- wallet.dlcConfig.stop()
    } yield ()
  }

  def destroyWalletAppConfig(
      walletAppConfig: WalletAppConfig
  )(implicit ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- walletAppConfig.dropTable("flyway_schema_history")
      _ <- walletAppConfig.dropAll()
      _ <- walletAppConfig.stop()
    } yield ()
  }

  /** Constructs callbacks for the wallet from the node to process blocks and
    * compact filters
    */
  def createNeutrinoNodeCallbacksForWallet(
      wallet: Wallet
  )(implicit system: ActorSystem): Future[NodeCallbacks] = {
    CallbackUtil.createNeutrinoNodeCallbacksForWallet(wallet)
  }

  /** Makes sure our wallet is fully funded with the default amounts specified
    * in [[BitcoinSWalletTest]]. This will future won't be completed until
    * balances satisfy [[isSameWalletBalances()]]
    */
  def awaitWalletBalances(
      fundedWallet: WalletWithBitcoind[?]
  )(implicit config: WalletAppConfig, system: ActorSystem): Future[Unit] = {
    AsyncUtil.retryUntilSatisfiedF(
      conditionF = () => isSameWalletBalances(fundedWallet),
      interval = 1.seconds,
      maxTries = 100
    )(system.dispatcher)
  }

  private def isSameWalletBalances(
      fundedWallet: WalletWithBitcoind[?]
  )(implicit config: WalletAppConfig, system: ActorSystem): Future[Boolean] = {
    import system.dispatcher
    val defaultAccount = config.defaultAccount
    val hdAccount1 = WalletTestUtil.getHdAccount1(config)
    val expectedDefaultAmt = BitcoinSWalletTest.expectedDefaultAmt
    val expectedAccount1Amt = BitcoinSWalletTest.expectedAccount1Amt
    val accountHandling = fundedWallet.wallet.accountHandling
    val defaultBalanceF = accountHandling.getBalance(defaultAccount)
    val account1BalanceF = accountHandling.getBalance(hdAccount1)
    for {
      balance <- defaultBalanceF
      account1Balance <- account1BalanceF
    } yield {
      balance == expectedDefaultAmt &&
      account1Balance == expectedAccount1Amt
    }
  }

  def buildBip39PasswordConfig(bip39PasswordOpt: Option[String]): Config = {
    bip39PasswordOpt match {
      case Some(bip39Password) =>
        if (bip39Password == "") {
          ConfigFactory.empty
        } else {
          val str = s"""bitcoin-s.keymanager.bip39password=$bip39Password"""
          ConfigFactory.parseString(str)
        }
      case None =>
        ConfigFactory.empty
    }
  }

  def buildBip39PasswordWithExtraConfig(
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config]
  ): Config = {
    val bip39PasswordConfig =
      BitcoinSWalletTest.buildBip39PasswordConfig(bip39PasswordOpt)

    val merged = bip39PasswordConfig.withFallback(
      extraConfig.getOrElse(ConfigFactory.empty)
    )

    merged
  }

  def getSegwitWalletConfigWithBip39PasswordOpt(
      pgUrl: Option[String]
  )(implicit system: ActorSystem): BitcoinSAppConfig = {
    val segwitConfig = BaseWalletTest.segwitWalletConf

    val extraConfig = BitcoinSWalletTest.buildBip39PasswordWithExtraConfig(
      bip39PasswordOpt = KeyManagerTestUtil.bip39PasswordOpt,
      extraConfig = Some(segwitConfig)
    )
    BaseWalletTest.getFreshConfig(() => pgUrl, Vector(extraConfig))
  }
}
