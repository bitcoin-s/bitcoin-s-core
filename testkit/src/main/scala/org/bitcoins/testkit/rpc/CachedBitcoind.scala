package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v27.BitcoindV27RpcClient
import org.bitcoins.rpc.util.{NodePair, NodeTriple}
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.BitcoinSPekkoAsyncTest

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.{Await, Future}

/** A trait that holds a cached instance of a
  * [[org.bitcoins.rpc.client.common.BitcoindRpcClient]] This is useful for
  * using with fixtures to avoid creating a new bitcoind everytime a new test is
  * run.
  *
  * The idea is our wallet/chain/node can just use the cached bitcoind rather
  * than a fresh one. This does mean that test cases have to be written in such
  * a way where assertions are not dependent on specific bitcoind state.
  */
trait CachedBitcoind[T <: BitcoindRpcClient] { self: BitcoinSPekkoAsyncTest =>

  /** Flag to indicate if the bitcoind was used
    *
    * If we don't have this, we have no way to know if we should cleanup the
    * cached bitcoind in the [[afterAll()]] method or not.
    *
    * We do not want to accidentally create a bitcoind inside of [[afterAll()]]
    * just to have it cleaned up in the same method.
    */
  protected val isBitcoindUsed: AtomicBoolean = new AtomicBoolean(false)

}

/** A cached bitcoind that has zero blocks in its chainstate. This is useful for
  * testing at certain times when we need to make sure bitcoind is ONLY
  * initialized, no chain state.
  */
trait CachedBitcoindNoFunds[T <: BitcoindRpcClient] extends CachedBitcoind[T] {
  self: BitcoinSPekkoAsyncTest =>
  def cachedBitcoind: Future[T]

  override def afterAll(): Unit = {
    if (isBitcoindUsed.get()) {
      // if it was used, shut down the cached bitcoind
      val stoppedF = for {
        cachedBitcoind <- cachedBitcoind
        _ <- BitcoindRpcTestUtil.stopServer(cachedBitcoind)
      } yield ()

      Await.result(stoppedF, duration)
    } else {
      // do nothing since bitcoind wasn't used
    }
  }
}

trait CachedBitcoindNoFundsNewest
    extends CachedBitcoindNoFunds[BitcoindRpcClient] {
  self: BitcoinSPekkoAsyncTest =>

  override lazy val cachedBitcoind: Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoind(Some(BitcoindVersion.newest))
  }
}

trait CachedBitcoindFunded[T <: BitcoindRpcClient] extends CachedBitcoind[T] {
  self: BitcoinSPekkoAsyncTest =>

  /** The bitcoind instance, lazyily created */
  protected lazy val cachedBitcoindWithFundsF: Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindWithFunds(None)
  }

  override def afterAll(): Unit = {
    if (isBitcoindUsed.get()) {
      // if it was used, shut down the cached bitcoind
      val stoppedF = for {
        _ <- Future.unit
        cachedBitcoind <- cachedBitcoindWithFundsF
        _ <- BitcoindRpcTestUtil.stopServer(cachedBitcoind)
      } yield {
        isBitcoindUsed.set(false)
        ()
      }

      Await.result(stoppedF, duration)
    } else {
      // do nothing since bitcoind wasn't used
    }
  }
}

trait CachedBitcoindV26 extends CachedBitcoindFunded[BitcoindRpcClient] {
  self: BitcoinSPekkoAsyncTest =>

  override protected lazy val cachedBitcoindWithFundsF
      : Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindWithFunds(Some(BitcoindVersion.V26))
  }
}

trait CachedBitcoindV27 extends CachedBitcoindFunded[BitcoindV27RpcClient] {
  self: BitcoinSPekkoAsyncTest =>

  override protected lazy val cachedBitcoindWithFundsF
      : Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindWithFunds(Some(BitcoindVersion.V27))
  }
}

trait CachedBitcoindNewest extends CachedBitcoindFunded[BitcoindRpcClient] {
  self: BitcoinSPekkoAsyncTest =>

  override protected lazy val cachedBitcoindWithFundsF
      : Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindWithFunds(Some(BitcoindVersion.newest))
  }
}

trait CachedBitcoindNewestNoP2pBlockFilters extends CachedBitcoindNewest {
  self: BitcoinSPekkoAsyncTest =>

  override protected lazy val cachedBitcoindWithFundsF
      : Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoind(Some(BitcoindVersion.newest), enableNeutrino = false)
  }
}

trait CachedBitcoindBlockFilterRpcNewest
    extends CachedBitcoindFunded[BitcoindRpcClient] {
  self: BitcoinSPekkoAsyncTest =>

  override protected lazy val cachedBitcoindWithFundsF
      : Future[BitcoindRpcClient] = {
    val _ = isBitcoindUsed.set(true)
    BitcoinSFixture
      .createBitcoindBlockFilterRpcWithFunds(Some(BitcoindVersion.newest))
  }
}

trait CachedBitcoindCollection[T <: BitcoindRpcClient]
    extends CachedBitcoind[T] {
  self: BitcoinSPekkoAsyncTest =>

  /** The version of bitcoind we are creating in the collection By default, we
    * just use the newest version of bitcoind
    */
  def version: BitcoindVersion

  /** Flag to indicate if the bitcoinds were used
    *
    * If we don't have this, we have no way to know if we should cleanup the
    * cached bitcoind in the [[afterAll()]] method or not.
    *
    * We do not want to accidentally create a bitcoind inside of [[afterAll()]]
    * just to have it cleaned up in the same method.
    */
  protected val isClientsUsed: AtomicBoolean = new AtomicBoolean(false)

  protected lazy val cachedClients
      : AtomicReference[Vector[BitcoindRpcClient]] = {
    new AtomicReference[Vector[BitcoindRpcClient]](Vector.empty)
  }

  override def afterAll(): Unit = {
    if (isClientsUsed.get()) {
      // if it was used, shut down the cached bitcoind
      val clients = cachedClients.get()
      val stoppedF = for {
        _ <- BitcoindRpcTestUtil.stopServers(clients)
      } yield ()

      Await.result(stoppedF, duration)
    } else {
      // do nothing since bitcoind wasn't used
    }
  }
}

trait CachedBitcoindPair[T <: BitcoindRpcClient]
    extends CachedBitcoindCollection[T] {
  self: BitcoinSPekkoAsyncTest =>

  lazy val clientsF: Future[NodePair[T]] = {
    BitcoindRpcTestUtil
      .createNodePair[T](version)
      .map(NodePair.fromTuple)
      .map { tuple =>
        isClientsUsed.set(true)
        val clients = cachedClients.get()
        cachedClients.set(clients ++ tuple.toVector)
        tuple
      }
  }
}

trait CachedBitcoindPairNewest
    extends CachedBitcoindCollection[BitcoindRpcClient] {
  self: BitcoinSPekkoAsyncTest =>

  override val version: BitcoindVersion = BitcoindVersion.newest

  lazy val clientsF: Future[NodePair[BitcoindRpcClient]] = {
    BitcoindRpcTestUtil
      .createNodePair[BitcoindRpcClient](version)
      .map(NodePair.fromTuple)
      .map { tuple =>
        isClientsUsed.set(true)
        val clients = cachedClients.get()
        cachedClients.set(clients ++ tuple.toVector)
        tuple
      }
  }
}

trait CachedBitcoindTriple[T <: BitcoindRpcClient]
    extends CachedBitcoindCollection[T] {
  self: BitcoinSPekkoAsyncTest =>

  lazy val clientsF: Future[NodeTriple[T]] = {
    BitcoindRpcTestUtil
      .createNodeTriple[T](version)
      .map(NodeTriple.fromTuple)
      .map { triple =>
        isClientsUsed.set(true)
        val clients = cachedClients.get()
        cachedClients.set(clients ++ triple.toVector)
        triple
      }
  }
}
