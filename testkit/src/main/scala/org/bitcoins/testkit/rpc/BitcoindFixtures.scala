package org.bitcoins.testkit.rpc

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddNodeArgument
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.util.{NodePair, NodeTriple}
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

/** A trait that is useful if you need bitcoind fixtures for your test suite */
trait BitcoindFixtures extends BitcoinSFixture with EmbeddedPg {
  self: BitcoinSAsyncFixtureTest =>

  def withNewestFundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient
  ): FutureOutcome = {
    makeDependentFixture[BitcoindRpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      }
    )(test)
  }

}

/** Bitcoind fixtures with a cached a bitcoind instance */
trait BitcoindFixturesCached extends BitcoindFixtures {
  self: BitcoinSAsyncFixtureTest & CachedBitcoind[?] =>
}

/** Bitcoind fixtures with a cached a bitcoind instance that is funded */
trait BitcoindFixturesFundedCached extends BitcoindFixtures {
  self: BitcoinSAsyncFixtureTest & CachedBitcoindFunded[?] =>

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewestFundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }
}

trait BitcoindFixturesFundedCachedNewest
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindNewest {
  override type FixtureParam = BitcoindRpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewestFundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Bitcoind fixtures with two cached bitcoind that are connected via p2p */
trait BitcoindFixturesCachedPair[T <: BitcoindRpcClient]
    extends BitcoindFixturesCached
    with CachedBitcoindPair[T] {
  self: BitcoinSAsyncFixtureTest =>

  def with2BitcoindsCached(
      test: OneArgAsyncTest,
      bitcoinds: NodePair[T]
  ): FutureOutcome = {
    makeDependentFixture[NodePair[T]](
      () => Future.successful(bitcoinds),
      destroy = { case _: NodePair[T] =>
        // do nothing since we are caching bitcoinds
        // the test trait may want to re-use them
        Future.unit
      }
    )(test)
  }

  def with2BitcoindsDisconnected(
      test: OneArgAsyncTest,
      bitcoinds: NodePair[T]
  ): FutureOutcome = {
    makeDependentFixture[NodePair[T]](
      () => {
        for {
          isConnected <- BitcoindRpcTestUtil.isConnected(bitcoinds)
          _ <-
            if (isConnected) {
              BitcoindRpcTestUtil.disconnectNodes(bitcoinds)
            } else Future.unit
          isNodeAdded <- BitcoindRpcTestUtil.isNodeAdded(bitcoinds)
          _ <-
            if (!isNodeAdded) {
              val node1 = bitcoinds.node1
              val node2Uri = bitcoinds.node2.getDaemon.uri
              node1.addNode(node2Uri, AddNodeArgument.Add)
            } else {
              Future.unit
            }
        } yield {
          bitcoinds
        }
      },
      destroy = { case nodePair: NodePair[T] =>
        // disconnect them in case the test case left them in a connected state
        for {
          isConnected <- BitcoindRpcTestUtil.isConnected(nodePair)
          _ <-
            if (isConnected) {
              BitcoindRpcTestUtil.disconnectNodes(nodePair)
            } else {
              Future.unit
            }
          isNodeAdded <- BitcoindRpcTestUtil.isNodeAdded(bitcoinds)
          _ <-
            if (isNodeAdded) {
              val node1 = bitcoinds.node1
              val node2Uri = bitcoinds.node2.getDaemon.uri
              node1.addNode(node2Uri, AddNodeArgument.Remove)
            } else {
              Future.unit
            }
        } yield {
          nodePair
        }
      }
    )(test)
  }
}

trait BitcoindFixturesCachedPairNewest
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindRpcClient] {
  override type FixtureParam = NodePair[BitcoindRpcClient]
  override val version: BitcoindVersion = BitcoindVersion.newest

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val futOutcome = for {
      pair <- clientsF
      futOutcome = with2BitcoindsCached(test, pair)
      f <- futOutcome.toFuture
    } yield f
    new FutureOutcome(futOutcome)
  }

  override def afterAll(): Unit = {
    super[BitcoindFixturesCachedPair].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** A pair of bitcoinds that are disconnected on the p2p network when given to
  * the test case
  */
trait BitcoindFixturesCachedPairNewestDisconnected
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindRpcClient] {
  override type FixtureParam = NodePair[BitcoindRpcClient]
  override val version: BitcoindVersion = BitcoindVersion.newest

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val futOutcome = for {
      pair <- clientsF
      futOutcome = with2BitcoindsDisconnected(test, pair)
      f <- futOutcome.toFuture
    } yield f
    new FutureOutcome(futOutcome)
  }

  override def afterAll(): Unit = {
    super[BitcoindFixturesCachedPair].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Bitcoind fixtures with three cached bitcoind that are connected via p2p */
trait BitcoindFixturesCachedTriple[T <: BitcoindRpcClient]
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCached
    with CachedBitcoindTriple[T] {

  def with3BitcoindsCached(
      test: OneArgAsyncTest,
      bitcoinds: NodeTriple[T]
  ): FutureOutcome = {
    makeDependentFixture[NodeTriple[T]](
      () => Future.successful(bitcoinds),
      destroy = { case _: NodeTriple[T] =>
        // do nothing since we are caching bitcoinds
        // the test trait may want to re-use them
        Future.unit
      }
    )(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindTriple].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}
