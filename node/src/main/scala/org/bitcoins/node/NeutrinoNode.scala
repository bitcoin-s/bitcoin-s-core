package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.apache.pekko.{Done, NotUsed}
import org.apache.pekko.actor.{ActorSystem, Cancellable}
import org.apache.pekko.stream.{
  ActorAttributes,
  OverflowStrategy,
  QueueOfferResult,
  Supervision
}
import org.apache.pekko.stream.scaladsl.{
  Keep,
  RunnableGraph,
  Source,
  SourceQueue,
  SourceQueueWithComplete
}
import org.bitcoins.core.api.node.{NodeType, Peer}
import org.bitcoins.core.p2p.{
  GetDataMessage,
  Inventory,
  NetworkMessage,
  TypeIdentifier
}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

case class NeutrinoNode(
    walletCreationTimeOpt: Option[Instant],
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem,
    paramPeers: Vector[Peer]
) extends Node
    with SourceQueue[NodeStreamMessage] {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node, got=${nodeConfig.nodeType}!"
  )

  private val isStarted: AtomicBoolean = new AtomicBoolean(false)
  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  implicit override def chainAppConfig: ChainAppConfig = chainConfig

  private val dataMessageStreamSource: Source[
    NodeStreamMessage,
    SourceQueueWithComplete[NodeStreamMessage]
  ] = {
    Source
      .queue[NodeStreamMessage](
        100 * nodeAppConfig.maxConnectedPeers,
        overflowStrategy = OverflowStrategy.backpressure,
        maxConcurrentOffers = Runtime.getRuntime.availableProcessors()
      )
  }

  override lazy val peerManager: PeerManager = {
    PeerManager(
      paramPeers = paramPeers,
      walletCreationTimeOpt = walletCreationTimeOpt,
      queue = this
    )
  }

  private var queueOpt: Option[SourceQueueWithComplete[NodeStreamMessage]] =
    None

  private var streamDoneFOpt: Option[Future[NodeState]] = None

  private val decider: Supervision.Decider = { case err: Throwable =>
    logger.error(s"Error occurred while processing p2p pipeline stream", err)
    Supervision.Resume
  }

  private def buildDataMessageStreamGraph(
      initState: NodeState,
      source: Source[NodeStreamMessage, NotUsed]
  ): RunnableGraph[Future[NodeState]] = {
    val graph = source
      .toMat(peerManager.buildP2PMessageHandlerSink(initState))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(decider))
    graph
  }

  override def start(): Future[NeutrinoNode] = {
    if (isStarted.get()) {
      logger.warn(s"NeutrinoNode already started")
      Future.successful(this)
    } else {
      isStarted.set(true)
      val (queue, source) =
        dataMessageStreamSource.preMaterialize()

      queueOpt = Some(queue)
      val peerFinder: PeerFinder =
        PeerFinder(
          peerManagerApi = peerManager,
          paramPeers = paramPeers,
          queue = queue
        )
      val initState =
        NodeState.NoPeers(waitingForDisconnection = Set.empty,
                          peerFinder,
                          cachedOutboundMessages = Vector.empty)

      val graph =
        buildDataMessageStreamGraph(initState = initState, source = source)
      val stateF = graph.run()
      streamDoneFOpt = Some(stateF)
      val res = for {
        node <- super.start()
        _ <- peerFinder.start()
        _ = {
          val inactivityCancellable = startHealthChecksJob()
          inactivityCancellableOpt = Some(inactivityCancellable)
        }
      } yield {
        node.asInstanceOf[NeutrinoNode]
      }

      res.failed.foreach(logger.error("Cannot start Neutrino node", _))

      res
    }

  }

  override def stop(): Future[NeutrinoNode] = {
    if (isStarted.get()) {
      logger.info(s"Stopping NeutrinoNode")
      isStarted.set(false)
      val start = System.currentTimeMillis()
      inactivityCancellableOpt.map(_.cancel())
      for {
        _ <- peerManager.stop()
        _ = queueOpt.map(_.complete())
        _ <- {
          val finishedF = streamDoneFOpt match {
            case Some(f) =>
              f.flatMap { case r: NodeRunningState =>
                r.peerFinder.stop()
              }
            case None => Future.successful(Done)
          }
          finishedF
        }
        _ = {
          // reset all variables
          streamDoneFOpt = None
          inactivityCancellableOpt = None
          queueOpt = None
        }
      } yield {
        logger.info(
          s"Node stopped! It took=${System.currentTimeMillis() - start}ms"
        )
        this
      }
    } else {
      logger.info(s"NeutrinoNode is already stopped")
      Future.successful(this)
    }

  }

  /** Fetches the given blocks from the peers and calls the appropriate
    * Backpressures if the queue is full
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256DigestBE]
  ): Future[Unit] = {
    if (blockHashes.isEmpty) {
      Future.unit
    } else {
      val typeIdentifier = TypeIdentifier.MsgWitnessBlock
      val inventories =
        blockHashes.map(hash => Inventory(typeIdentifier, hash.flip))
      val message = GetDataMessage(inventories)
      val networkMessage = NetworkMessage(nodeAppConfig.network, message)
      offer(NodeStreamMessage.SendToPeer(networkMessage, None))
        .map(_ => ())
    }
  }

  /** Starts to sync our node with our peer If our local best block hash is the
    * same as our peers we will not sync, otherwise we will keep syncing until
    * our best block hashes match up
    *
    * @return
    */
  override def sync(): Future[Unit] = {
    // wait for a peer to be available to sync from...
    // due to underlying mutability in PeerManager/PeerFinder
    // we may not have a peer available for selection immediately
    val peerAvailableF =
      AsyncUtil.retryUntilSatisfiedF(() => getConnectionCount.map(_ > 0))
    for {
      _ <- peerAvailableF
      _ <- peerManager.sync(None)
    } yield ()
  }

  /** Gets the number of compact filters in the database */
  override def getFilterCount(): Future[Int] =
    chainApiFromDb().flatMap(_.getFilterCount())

  /** Returns the block height of the given block stamp */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    chainApiFromDb().flatMap(_.getHeightByBlockStamp(blockStamp))

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int
  ): Future[Vector[FilterResponse]] =
    chainApiFromDb().flatMap(_.getFiltersBetweenHeights(startHeight, endHeight))

  private val INACTIVITY_CHECK_TIMEOUT = 60.seconds

  @volatile private var inactivityCancellableOpt: Option[Cancellable] =
    None

  private def healthChecksRunnable(): Runnable = { () =>
    val peers = peerManager.peers
    logger.debug(s"Running health checks for peers=${peers}")
    val resultF = if (peers.nonEmpty || isStarted.get) {
      queueOpt match {
        case Some(q) =>
          q.offer(NodeStreamMessage.PeerHealthCheck)
            .map(_ => ())
        case None =>
          logger.warn(s"No queue defined for inactivity check")
          Future.unit
      }
    } else {
      start().map(_ => ())
    }

    resultF.failed.foreach(err =>
      logger.error(s"Failed to run inactivity checks for peers=${peers}", err))

    Await.result(resultF, INACTIVITY_CHECK_TIMEOUT)
  }

  private def startHealthChecksJob(): Cancellable = {
    val interval = nodeAppConfig.healthCheckInterval
    system.scheduler.scheduleAtFixedRate(
      initialDelay = interval,
      interval = interval
    )(healthChecksRunnable())
  }

  override def offer(elem: NodeStreamMessage): Future[QueueOfferResult] = {
    queueOpt match {
      case Some(queue) => queue.offer(elem)
      case None =>
        Future.failed(
          new RuntimeException(
            s"NeutrinoNode not started, cannot process p2p message until NeutrinoNode.start() is called"
          )
        )
    }
  }

  override def watchCompletion(): Future[Done] = {
    queueOpt match {
      case Some(queue) => queue.watchCompletion()
      case None        => Future.successful(Done)
    }
  }
}
