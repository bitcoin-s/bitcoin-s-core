package org.bitcoins.testkit.node

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.AppConfig
import org.bitcoins.node.{SpvNode, SpvNodeCallbacks}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.{
  PeerHandler,
  PeerMessageReceiver,
  PeerMessageSender
}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.scalatest.{
  BeforeAndAfter,
  BeforeAndAfterAll,
  FutureOutcome,
  MustMatchers
}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest
    extends BitcoinSFixture
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.nodeConf)
  }

  override def afterAll(): Unit = {
    system.terminate()
    ()
  }

  implicit lazy val system: ActorSystem = {
    ActorSystem(s"${getClass.getSimpleName}-${System.currentTimeMillis}")
  }

  implicit lazy val ec: ExecutionContext =
    system.dispatcher

  val timeout: FiniteDuration = 10.seconds

  /** Wallet config with data directory set to user temp directory */
  implicit protected lazy val config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getTestConfig()

  implicit protected lazy val chainConfig: ChainAppConfig = config.chainConf

  implicit protected lazy val nodeConfig: NodeAppConfig = config.nodeConf

  implicit lazy val np: NetworkParameters = config.nodeConf.network

  lazy val startedBitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  lazy val bitcoindPeerF = startedBitcoindF.map(NodeTestUtil.getBitcoindPeer)

  def withSpvNode(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {

    val spvBuilder: () => Future[SpvNode] = { () =>
      val bitcoindF = BitcoinSFixture.createBitcoind()
      bitcoindF.flatMap { bitcoind =>
        NodeUnitTest
          .createSpvNode(bitcoind, SpvNodeCallbacks.empty)(system,
                                                           chainConfig,
                                                           nodeConfig)
          .flatMap(_.start())
      }
    }

    makeDependentFixture(
      build = spvBuilder,
      destroy = NodeUnitTest.destroySpvNode
    )(test)
  }

  def withSpvNodeConnectedToBitcoind(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {
    val spvWithBitcoindBuilder: () => Future[SpvNodeConnectedWithBitcoind] = {
      () =>
        val bitcoindF = BitcoinSFixture.createBitcoind()
        bitcoindF.flatMap { bitcoind =>
          val spvNode = NodeUnitTest
            .createSpvNode(bitcoind, SpvNodeCallbacks.empty)(system,
                                                             chainConfig,
                                                             nodeConfig)
          val startedSpv = spvNode
            .flatMap(_.start())

          startedSpv.map(spv => SpvNodeConnectedWithBitcoind(spv, bitcoind))
        }
    }

    makeDependentFixture(
      build = spvWithBitcoindBuilder,
      destroy = NodeUnitTest.destorySpvNodeConnectedWithBitcoind
    )(test)
  }

  def withSpvNodeFundedWalletBitcoind(
      test: OneArgAsyncTest,
      callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem): FutureOutcome = {

    makeDependentFixture(
      build = () => NodeUnitTest.createSpvNodeFundedWalletBitcoind(callbacks),
      destroy = NodeUnitTest.destroySpvNodeFundedWalletBitcoind
    )(test)
  }
}

object NodeUnitTest extends BitcoinSLogger {

  /** Represents a spv node, a funded bitcoin-s wallet, and a bitcoind instance that is running */
  case class SpvNodeFundedWalletBitcoind(
      spvNode: SpvNode,
      wallet: UnlockedWalletApi,
      bitcoindRpc: BitcoindRpcClient)

  def destroySpvNode(spvNode: SpvNode)(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    val stopF = spvNode.stop()
    stopF.flatMap(_ => ChainUnitTest.destroyHeaderTable())
  }

  def destorySpvNodeConnectedWithBitcoind(
      spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    import system.dispatcher
    val spvNode = spvNodeConnectedWithBitcoind.spvNode
    val bitcoind = spvNodeConnectedWithBitcoind.bitcoind
    val spvNodeDestroyF = destroySpvNode(spvNode)
    val bitcoindDestroyF = ChainUnitTest.destroyBitcoind(bitcoind)

    for {
      _ <- spvNodeDestroyF
      _ <- bitcoindDestroyF
    } yield ()
  }

  /** Creates a spv node, a funded bitcoin-s wallet, all of which are connected to bitcoind */
  def createSpvNodeFundedWalletBitcoind(callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[SpvNodeFundedWalletBitcoind] = {
    import system.dispatcher
    val fundedWalletF = BitcoinSWalletTest.fundedWalletAndBitcoind()
    for {
      fundedWallet <- fundedWalletF
      spvNode <- createSpvNode(fundedWallet.bitcoind, callbacks)
    } yield {
      SpvNodeFundedWalletBitcoind(spvNode = spvNode,
                                  wallet = fundedWallet.wallet,
                                  bitcoindRpc = fundedWallet.bitcoind)
    }
  }

  def destroySpvNodeFundedWalletBitcoind(
      fundedWalletBitcoind: SpvNodeFundedWalletBitcoind)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    import system.dispatcher
    val walletWithBitcoind = {
      BitcoinSWalletTest.WalletWithBitcoind(fundedWalletBitcoind.wallet,
                                            fundedWalletBitcoind.bitcoindRpc)
    }
    val destroyedF = for {
      _ <- destroySpvNode(fundedWalletBitcoind.spvNode)
      _ <- BitcoinSWalletTest.destroyWalletWithBitcoind(walletWithBitcoind)
    } yield ()

    destroyedF

  }

  def buildPeerMessageReceiver()(
      implicit system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): PeerMessageReceiver = {
    import system.dispatcher
    val dao = BlockHeaderDAO()
    val chainHandler = ChainHandler(dao)
    val receiver =
      PeerMessageReceiver.newReceiver(chainHandler, SpvNodeCallbacks.empty)
    receiver
  }

  def buildPeerHandler(peer: Peer)(
      implicit system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): PeerHandler = {
    val peerMsgReceiver = buildPeerMessageReceiver()
    //the problem here is the 'self', this needs to be an ordinary peer message handler
    //that can handle the handshake
    val peerMsgSender: PeerMessageSender = {
      val client = NodeTestUtil.client(peer, peerMsgReceiver)
      PeerMessageSender(client)
    }
    PeerHandler(peerMsgReceiver, peerMsgSender)

  }

  def peerSocketAddress(
      bitcoindRpcClient: BitcoindRpcClient): InetSocketAddress = {
    NodeTestUtil.getBitcoindSocketAddress(bitcoindRpcClient)
  }

  def createPeer(bitcoind: BitcoindRpcClient): Peer = {
    val socket = peerSocketAddress(bitcoind)
    Peer(id = None, socket = socket)
  }

  def createSpvNode(bitcoind: BitcoindRpcClient, callbacks: SpvNodeCallbacks)(
      implicit system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[SpvNode] = {
    import system.dispatcher
    val chainApiF = ChainUnitTest.createChainHandler()
    val peer = createPeer(bitcoind)
    for {
      chainApi <- chainApiF
    } yield {
      SpvNode(peer = peer,
              chainApi = chainApi,
              bloomFilter = NodeTestUtil.emptyBloomFilter,
              callbacks = callbacks)
    }
  }

}
