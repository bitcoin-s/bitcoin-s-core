package org.bitcoins.testkit.chain.fixture

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest}
import org.bitcoins.testkit.rpc.{CachedBitcoind, CachedBitcoindNewest}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

/** Chain unit test that requires a cached bitcoind type to be injected */
trait ChainWithBitcoindUnitTest extends ChainDbUnitTest {
  self: CachedBitcoind[_] =>

}

trait ChainWithBitcoindNewestCachedUnitTest
    extends ChainWithBitcoindUnitTest
    with CachedBitcoindNewest {

  override type FixtureParam = BitcoindBaseVersionChainHandlerViaRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withBitcoindNewestChainHandlerViaRpc(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withBitcoindNewestChainHandlerViaRpc(
      test: OneArgAsyncTest,
      bitcoindRpcClient: BitcoindRpcClient
  ): FutureOutcome = {
    val builder: () => Future[BitcoindBaseVersionChainHandlerViaRpc] = { () =>
      ChainUnitTest.createChainApiWithBitcoindRpc(bitcoindRpcClient)
    }
    val destroy: BitcoindBaseVersionChainHandlerViaRpc => Future[Unit] = {
      case _: BitcoindBaseVersionChainHandlerViaRpc =>
        ChainUnitTest.destroyChainApi()
    }
    makeDependentFixture(builder, destroy)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[ChainWithBitcoindUnitTest].afterAll()
  }
}
