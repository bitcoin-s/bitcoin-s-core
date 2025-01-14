package org.bitcoins.testkit.node

import com.typesafe.config.Config
import org.apache.pekko.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.api.tor.Socks5ProxyParams
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.{NeutrinoNode, Node, P2PLogger}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.util.TorUtil

import java.net.{InetSocketAddress, URI}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

abstract class NodeTestUtil extends P2PLogger {

  /** Helper method to get the [[java.net.InetSocketAddress]] we need to connect
    * to make a p2p connection with bitcoind
    * @param bitcoindRpcClient
    * @return
    */
  def getBitcoindSocketAddress(
      bitcoindRpcClient: BitcoindRpcClient
  )(implicit executionContext: ExecutionContext): Future[InetSocketAddress] = {
    if (TorUtil.torEnabled) {
      for {
        networkInfo <- bitcoindRpcClient.getNetworkInfo
      } yield {
        val onionAddress = networkInfo.localaddresses
          .find(_.address.endsWith(".onion"))
          .getOrElse(
            throw new IllegalArgumentException(
              s"bitcoind instance is not configured to use Tor: ${bitcoindRpcClient}"
            )
          )

        InetSocketAddress.createUnresolved(
          onionAddress.address,
          onionAddress.port
        )

      }
    } else {
      val instance = bitcoindRpcClient.instance
      Future.successful(
        new InetSocketAddress(instance.uri.getHost, instance.p2pPort)
      )
    }
  }

  def getSocks5ProxyParams: Option[Socks5ProxyParams] = {
    if (TorUtil.torEnabled) {
      Some(
        Socks5ProxyParams(
          address = InetSocketAddress.createUnresolved("127.0.0.1", 9050),
          credentialsOpt = None,
          randomizeCredentials = true
        )
      )
    } else None
  }

  /** Gets the [[Peer]] that corresponds to
    * [[org.bitcoins.rpc.client.common.BitcoindRpcClient]]
    */
  def getBitcoindPeer(
      bitcoindRpcClient: BitcoindRpcClient
  )(implicit executionContext: ExecutionContext): Future[Peer] =
    for {
      socket <- getBitcoindSocketAddress(bitcoindRpcClient)
    } yield {
      val socks5ProxyParams = getSocks5ProxyParams
      Peer(socket, socks5ProxyParams = socks5ProxyParams)
    }

  /** Checks if the given node and bitcoind is synced */
  def isSameBestHash(node: Node, rpc: BitcoindRpcClient)(implicit
      ec: ExecutionContext
  ): Future[Boolean] = {
    val hashF = rpc.getBestBlockHash()
    for {
      chainApi <- node.chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash()
      hash <- hashF
    } yield {
      bestHash == hash
    }
  }

  private def isSameBestFilter(
      node: NeutrinoNode,
      rpc: BitcoindRpcClient,
      bestBlockHashBEOpt: Option[DoubleSha256DigestBE]
  )(implicit ec: ExecutionContext): Future[Boolean] = {

    val bestBlockHashBEF = bestBlockHashBEOpt match {
      case Some(bestBlockHash) => Future.successful(bestBlockHash)
      case None                => rpc.getBestBlockHash()
    }

    for {
      bestBlockHashBE <- bestBlockHashBEF
      chainApi <- node.chainApiFromDb()
      bestFilterOpt <- chainApi.getBestFilter()
    } yield {
      bestFilterOpt match {
        case Some(bestFilter) => bestFilter.blockHashBE == bestBlockHashBE
        case None             => false
      }
    }
  }

  def isSameBestFilterHeight(node: NeutrinoNode, rpc: BitcoindRpcClient)(
      implicit ec: ExecutionContext
  ): Future[Boolean] = {
    val rpcCountF = rpc.getBlockCount()
    for {
      chainApi <- node.chainApiFromDb()
      filterCount <- chainApi.getFilterCount()
      blockCount <- rpcCountF
    } yield {
      blockCount == filterCount
    }
  }

  def isSameBestFilterHeaderHeight(node: NeutrinoNode, rpc: BitcoindRpcClient)(
      implicit ec: ExecutionContext
  ): Future[Boolean] = {
    val rpcCountF = rpc.getBlockCount()
    for {
      chainApi <- node.chainApiFromDb()
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      blockCount <- rpcCountF
    } yield {
      blockCount == filterHeaderCount
    }
  }

  /** Checks if the given light client and bitcoind has the same number of
    * blocks in their blockchains
    */
  def isSameBlockCount(node: Node, rpc: BitcoindRpcClient)(implicit
      ec: ExecutionContext
  ): Future[Boolean] = {
    val rpcCountF = rpc.getBlockCount()
    for {
      chainApi <- node.chainApiFromDb()
      count <- chainApi.getBlockCount()
      rpcCount <- rpcCountF
    } yield {
      rpcCount == count
    }
  }

  private val syncTries: Int = 15

  /** Awaits sync between the given node and bitcoind client */
  def awaitSync(node: NeutrinoNode, rpc: BitcoindRpcClient)(implicit
      sys: ActorSystem
  ): Future[Unit] = {
    awaitAllSync(node, rpc)
  }

  /** Awaits sync between the given node and bitcoind client */
  def awaitCompactFilterHeadersSync(node: NeutrinoNode, rpc: BitcoindRpcClient)(
      implicit sys: ActorSystem
  ): Future[Unit] = {
    import sys.dispatcher
    TestAsyncUtil
      .retryUntilSatisfiedF(
        () => isSameBestFilterHeaderHeight(node, rpc),
        1.second,
        maxTries = syncTries
      )
  }

  /** Awaits sync between the given node and bitcoind client */
  def awaitCompactFiltersSync(
      node: NeutrinoNode,
      rpc: BitcoindRpcClient,
      bestBlockHashBEOpt: Option[DoubleSha256DigestBE] = None
  )(implicit sys: ActorSystem): Future[Unit] = {
    import sys.dispatcher
    TestAsyncUtil
      .retryUntilSatisfiedF(
        () => isSameBestFilter(node, rpc, bestBlockHashBEOpt),
        1.second,
        maxTries = syncTries
      )
  }

  /** The future doesn't complete until the nodes best hash is the given hash */
  def awaitBestHash(
      node: Node,
      bitcoind: BitcoindRpcClient,
      bestHashOpt: Option[DoubleSha256DigestBE] = None
  )(implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    def bestBitcoinSHashF: Future[DoubleSha256DigestBE] = {
      node.chainApiFromDb().flatMap(_.getBestBlockHash())
    }
    def bestBitcoindHashF: Future[DoubleSha256DigestBE] = {
      bestHashOpt match {
        case Some(bestHash) => Future.successful(bestHash)
        case None           => bitcoind.getBestBlockHash()
      }
    }

    TestAsyncUtil.retryUntilSatisfiedF(
      () => {
        for {
          bestBitcoindHash <- bestBitcoindHashF
          bestBitcoinSHash <- bestBitcoinSHashF
        } yield {
          bestBitcoinSHash == bestBitcoindHash
        }
      },
      interval = 1.second,
      maxTries = syncTries
    )
  }

  /** Awaits header, filter header and filter sync between the neutrino node and
    * rpc client
    * @param the
    *   node we are syncing
    * @param bitcoind
    *   the node we are syncing against
    * @param bestBlockHashBE
    *   the best block hash we are expected to sync to, this is useful for reorg
    *   situations. If None given, we use bitcoind's best block header
    */
  def awaitAllSync(
      node: NeutrinoNode,
      bitcoind: BitcoindRpcClient,
      bestBlockHashBE: Option[DoubleSha256DigestBE] = None
  )(implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    for {
      _ <- NodeTestUtil.awaitBestHash(node, bitcoind, bestBlockHashBE)
      _ <- NodeTestUtil.awaitCompactFilterHeadersSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind, bestBlockHashBE)
    } yield ()
  }

  def awaitSyncAndIBD(node: NeutrinoNode, bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem
  ): Future[Unit] = {
    import system.dispatcher

    for {
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- AsyncUtil.retryUntilSatisfiedF(
        () => {
          val chainApi = node.chainApiFromDb()
          val syncingF = chainApi.flatMap(_.isSyncing())
          val isIBDF = chainApi.flatMap(_.isIBD())
          for {
            syncing <- syncingF
            isIBD <- isIBDF
          } yield {
            !syncing && !isIBD
          }
        },
        interval = 1.second,
        maxTries = 5
      )
    } yield ()

  }

  /** returns a Future that isn't completed until the peer manager has
    * [[expectedConnectionCount]] connections
    */
  def awaitConnectionCount(
      node: Node,
      expectedConnectionCount: Int,
      interval: FiniteDuration = 1.second,
      maxTries: Int = 30
  )(implicit ec: ExecutionContext): Future[Unit] = {
    AsyncUtil.retryUntilSatisfiedF(
      () => node.getConnectionCount.map(_ == expectedConnectionCount),
      interval = interval,
      maxTries = maxTries
    )
  }

  /** get our neutrino node's uri from a test bitcoind instance to send rpc
    * commands for our node. The peer must be initialized by the node.
    */
  def getNodeURIFromBitcoind(
      bitcoind: BitcoindRpcClient,
      localAddressBitcoinS: InetSocketAddress
  )(implicit system: ActorSystem): Future[URI] = {
    import system.dispatcher
    bitcoind.getPeerInfo.map { peerInfo =>
      val localFilter = peerInfo.filter { p =>
        p.networkInfo.addrlocal.isDefined && p.subver.contains(
          NodeConstants.userAgent
        ) && p.networkInfo.addr.getPort == localAddressBitcoinS.getPort
      }
      val result = localFilter.head.networkInfo.addr
      result
    }
  }

  def disconnectNode(bitcoind: BitcoindRpcClient, node: NeutrinoNode)(implicit
      system: ActorSystem
  ): Future[Unit] = {
    import system.dispatcher
    for {
      peer <- NodeTestUtil.getBitcoindPeer(bitcoind)
      address <- node.peerManager
        .getPeerData(peer)
        .get
        .peerConnection
        .getLocalAddress
        .map(_.get)
      uri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoind, address)
      _ <- bitcoind.disconnectNode(uri)
      _ <- NodeTestUtil.awaitConnectionCount(node, 0)
    } yield ()
  }

  def getStartedNodeCustomConfig(initNode: NeutrinoNode, config: Config)(
      implicit ec: ExecutionContext
  ): Future[NeutrinoNode] = {
    val stoppedConfigF = for {
      _ <- initNode.stop()
      _ <- initNode.nodeConfig.stop()
    } yield ()
    val newNodeAppConfigF =
      stoppedConfigF.map(_ => initNode.nodeConfig.withOverrides(config))
    val nodeF = {
      for {
        newNodeAppConfig <- newNodeAppConfigF
        _ <- newNodeAppConfig.start()
      } yield {
        NeutrinoNode(
          walletCreationTimeOpt = initNode.walletCreationTimeOpt,
          nodeConfig = newNodeAppConfig,
          chainConfig = initNode.chainAppConfig,
          actorSystem = initNode.system,
          paramPeers = initNode.paramPeers
        )
      }
    }

    val startedF = nodeF.flatMap(_.start())
    startedF
  }
}

object NodeTestUtil extends NodeTestUtil
