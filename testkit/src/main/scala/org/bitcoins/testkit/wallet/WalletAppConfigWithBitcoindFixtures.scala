package org.bitcoins.testkit.wallet

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCached,
  CachedBitcoind,
  CachedBitcoindNewest
}
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

trait WalletAppConfigWithBitcoindFixtures
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCached
    with EmbeddedPg { self: CachedBitcoind[_] =>

  override def afterAll(): Unit = {
    super[EmbeddedPg].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

trait WalletAppConfigWithBitcoindNewestFixtures
    extends WalletAppConfigWithBitcoindFixtures
    with CachedBitcoindNewest {

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[WalletAppConfigWithBitcoindFixtures].afterAll()
  }

  override type FixtureParam = WalletAppConfigWithBitcoindRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withWalletAppConfigBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withWalletAppConfigBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient
  ): FutureOutcome = {
    makeDependentFixture[WalletAppConfigWithBitcoindRpc](
      () => {
        val walletConfig =
          BaseWalletTest.getFreshWalletAppConfig(() => pgUrl(), Vector.empty)
        for {
          _ <- walletConfig.start()
          model = WalletAppConfigWithBitcoindRpc(walletConfig, bitcoind)
        } yield model
      },
      { case walletAppConfigWithBitcoindRpc =>
        BitcoinSWalletTest.destroyWalletAppConfig(
          walletAppConfigWithBitcoindRpc.walletAppConfig
        )
      }
    )(test)
  }
}
