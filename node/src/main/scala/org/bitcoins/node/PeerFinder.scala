package org.bitcoins.node

import org.apache.pekko.actor.{ActorSystem, Cancellable}
import org.apache.pekko.stream.scaladsl.SourceQueue
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.{Peer, PeerManagerApi}
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3}
import org.bitcoins.core.p2p.{ServiceIdentifier, VersionMessage}
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{PeerDAO, PeerDb}
import org.bitcoins.node.networking.peer.{
  ControlMessageHandler,
  PeerConnection,
  PeerMessageSender
}
import org.bitcoins.node.util.BitcoinSNodeUtil

import java.net.{InetAddress, UnknownHostException}
import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Random, Success}

case class PeerFinder(
    peerManagerApi: PeerManagerApi,
    paramPeers: Vector[Peer],
    queue: SourceQueue[NodeStreamMessage]
)(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig
) extends StartStopAsync[PeerFinder]
    with P2PLogger {

  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  val controlMessageHandler: ControlMessageHandler =
    ControlMessageHandler(this)

  /** Returns peers by querying each dns seed once. These will be IPv4
    * addresses.
    */
  private lazy val getPeersFromDnsSeeds: Future[Vector[Peer]] = {
    val dnsSeeds = nodeAppConfig.network.dnsSeeds
    val addressesF: Future[Vector[String]] = Future
      .traverse(dnsSeeds) { seed =>
        Future {
          try {
            val r = InetAddress
              .getAllByName(seed)
              .toVector
            r
          } catch {
            case _: UnknownHostException =>
              logger.debug(s"DNS seed $seed is unavailable.")
              Vector.empty[InetAddress]
          }
        }
      }
      .map(_.flatten.distinct.map(_.getHostAddress).toVector)
    addressesF.map(BitcoinSNodeUtil.stringsToPeers(_))

  }

  /** Returns peers from hardcoded addresses taken from
    * https://github.com/bitcoin/bitcoin/blob/master/contrib/seeds/nodes_main.txt
    */
  private def getPeersFromResources: Vector[Peer] = {
    nodeAppConfig.network match {
      case MainNet =>
        val source =
          Source.fromURL(getClass.getResource("/hardcoded-peers.txt"))
        val addresses = source
          .getLines()
          .toVector
          .filter(nodeAppConfig.torConf.enabled || !_.contains(".onion"))
        val peers = BitcoinSNodeUtil.stringsToPeers(addresses)
        Random.shuffle(peers)
      case TestNet3 | RegTest | SigNet =>
        Vector.empty

    }

  }

  /** Returns tuple (non-filter peer, filter peers) from all peers stored in
    * database
    */
  private def getPeersFromDb: Future[(Vector[PeerDb], Vector[PeerDb])] = {
    val dbF: Future[Vector[PeerDb]] =
      PeerDAO().findAllWithTorFilter(nodeAppConfig.torConf.enabled)

    val partitionF = dbF.map(
      _.partition(b =>
        !ServiceIdentifier.fromBytes(b.serviceBytes).nodeCompactFilters)
    )

    partitionF.map { p =>
      val sorted1 = p._1.sortBy(_.lastSeen).reverse
      val sorted2 = p._2.sortBy(_.lastSeen).reverse
      (sorted1, sorted2)
    }
  }

  /** Gets last seen peers before a given cool down a period so we don't keep
    * automatically reconnecting to peers we just disconnected
    */
  private def getLastSeenBlockFilterPeers(
      dbSlots: Int
  ): Future[Vector[PeerDb]] = {
    val cooldown = Instant
      .now()
      .minusMillis(nodeAppConfig.connectionAttemptCooldownPeriod.toMillis)
    for {
      potentialPeerDbs <- getPeersFromDb.map(_._2)
      filtered = potentialPeerDbs.filter(_.lastSeen.isBefore(cooldown))
    } yield Random.shuffle(filtered).take(dbSlots)
  }

  /** Returns peers from bitcoin-s.config file unless peers are supplied as an
    * argument to [[PeerManager]] in which case it returns those.
    */
  private def getPeersFromConfig: Vector[Peer] = {
    val addresses = nodeAppConfig.peers.filter(p =>
      nodeAppConfig.torConf.enabled || !p.toString.contains(".onion"))
    addresses
  }

  // for the peers we try
  private val _peerData: mutable.Map[Peer, PeerData] = {
    mutable.Map.empty
  }

  private val _peersToTry: PeerStack = PeerStack()

  private val maxPeerSearchCount: Int = 8

  private val maxStackPush: Int = maxPeerSearchCount * maxPeerSearchCount

  private val initialDelay: FiniteDuration = nodeAppConfig.tryPeersStartDelay

  private val isConnectionSchedulerRunning = new AtomicBoolean(false)

  private var peerConnectionCancellableOpt: Option[Cancellable] = None

  private def peerConnectionScheduler(): Cancellable = {
    system.scheduler.scheduleWithFixedDelay(
      initialDelay = initialDelay,
      delay = nodeAppConfig.tryNextPeersInterval
    ) { () =>
      {
        queryForPeerConnections(excludePeers = Set.empty)
        ()
      }
    }
  }

  override def start(): Future[PeerFinder] = {
    if (!isStarted.get()) {
      logger.info(
        s"Starting PeerFinder initialDelay=${initialDelay.toSeconds} seconds tryPeersInterval=${nodeAppConfig.tryNextPeersInterval.toMinutes} minutes paramPeers=$paramPeers"
      )
      val start = System.currentTimeMillis()
      isStarted.set(true)
      val peersToTry = (paramPeers ++ getPeersFromConfig).distinct
      val pds = peersToTry.map(p => buildPeerData(p, isPersistent = true))
      // higher priority for param peers
      _peersToTry.pushAll(pds, priority = 2)

      val peerDiscoveryF = if (nodeAppConfig.enablePeerDiscovery) {
        val startedF = for {
          (dbNonCfPeerDb, dbCfPeerDb) <- getPeersFromDb
          dnsF = {
            if (dbNonCfPeerDb.isEmpty && dbCfPeerDb.isEmpty) {
              getPeersFromDnsSeeds
            } else {
              Future.successful(Vector.empty)
            }
          }
          dbNonCf = dbNonCfPeerDb.map(_.peer(nodeAppConfig.socks5ProxyParams))
          dbCf = dbCfPeerDb
            .take(maxStackPush)
            .map(_.peer(nodeAppConfig.socks5ProxyParams))
          dns <- dnsF
          peersDbs = {
            if (dbNonCf.isEmpty) {
              val shuffle = Random.shuffle(
                (dns ++ getPeersFromResources).take(maxStackPush))
              shuffle
            } else {
              (dbNonCf ++ getPeersFromResources).take(maxStackPush)
            }

          }
        } yield {
          val pds = peersDbs.map(p => buildPeerData(p, isPersistent = false))
          _peersToTry.pushAll(pds)
          val dbPds = dbCf.map(p => buildPeerData(p, isPersistent = false))
          _peersToTry.pushAll(dbPds, priority = 1)
          peerConnectionCancellableOpt = Some(peerConnectionScheduler())
          this
        }

        startedF
      } else {
        logger.info("Peer discovery disabled.")
        peerConnectionCancellableOpt = Some(peerConnectionScheduler())
        Future.successful(this)
      }

      for {
        peerFinder <- peerDiscoveryF
        _ = logger.info(
          s"Done starting PeerFinder, it took ${System.currentTimeMillis() - start}ms"
        )
      } yield peerFinder
    } else {
      logger.warn(s"PeerFinder already started")
      Future.successful(this)
    }
  }

  def connect(peer: Peer): Future[Unit] = {
    logger.info(s"Attempting to connect peer=$peer")
    if (isStarted.get()) {
      tryPeer(peer, isPersistent = true)
    } else {
      logger.warn(
        s"Ignoring connect attempt to peer=$peer as PeerFinder is not started"
      )
      Future.unit
    }
  }

  def reconnect(peer: Peer): Future[Unit] = {
    logger.info(s"Attempting to reconnect peer=$peer")
    if (isStarted.get) {
      tryToReconnectPeer(peer)
    } else {
      logger.warn(
        s"Ignoring reconnect attempt to peer=$peer as PeerFinder is not started"
      )
      Future.unit
    }
  }

  override def stop(): Future[PeerFinder] = {
    if (isStarted.get()) {
      logger.info(s"Stopping PeerFinder")
      isStarted.set(false)
      // stop scheduler
      peerConnectionCancellableOpt.map(_.cancel())
      peerConnectionCancellableOpt = None
      // delete try queue
      _peersToTry.clear()

      val stopF = for {
        _ <- Future.traverse(_peerData.map(_._1))(removePeer(_))
        _ <- AsyncUtil
          .retryUntilSatisfied(
            {
              // there seems to be some sort of bug in mutable.Map.isEmpty
              // convert it to an immutable Map with .toMap and then check isEmpty
              _peerData.toMap.isEmpty
            },
            interval = 1.seconds,
            maxTries = 30
          )
      } yield {
        logger.info(s"Done stopping PeerFinder")
        this
      }

      stopF.failed.foreach { e =>
        logger.error(
          s"Failed to stop peer finder. Peers: ${_peerData.toMap}",
          e
        )
      }
      stopF
    } else {
      logger.warn(s"PeerFinder already stopped")
      Future.successful(this)
    }

  }

  /** creates and initialises a new test peer */
  private def tryPeer(peer: Peer, isPersistent: Boolean): Future[Unit] = {
    logger.debug(s"tryPeer=$peer")
    val peerConnection = PeerConnection(peer, queue)
    val peerMessageSender = PeerMessageSender(peerConnection)
    val pd = isPersistent match {
      case true  => PersistentPeerData(peer, peerMessageSender)
      case false => AttemptToConnectPeerData(peer, peerMessageSender)
    }
    _peerData.put(peer, pd)
    peerConnection.connect()
  }

  private def tryToReconnectPeer(peer: Peer): Future[Unit] = {
    val peerConnection = PeerConnection(peer, queue)
    val peerMessageSender = PeerMessageSender(peerConnection)
    _peerData.put(peer, PersistentPeerData(peer, peerMessageSender))
    peerConnection.reconnect()

  }

  def removePeer(peer: Peer): Future[Option[PeerData]] = {
    Future.successful {
      logger.debug(s"Removing peer=$peer")
      _peerData.remove(peer) // peer must be a member of _peerData
    }
  }

  def setServiceIdentifier(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier
  ): Unit = {
    _peerData(peer).setServiceIdentifier(serviceIdentifier)
  }

  def popFromCache(peer: Peer): Option[PeerData] = {
    _peerData.remove(peer)
  }

  def hasPeer(peer: Peer): Boolean = {
    _peerData.contains(peer)
  }

  def getPeerData(peer: Peer): Option[PeerData] = {
    _peerData.get(peer)
  }

  def addToTry(peers: Vector[PeerData], priority: Int = 0): Unit = {
    _peersToTry.pushAll(peers, priority)
  }

  def onVersionMessage(peer: Peer, versionMsg: VersionMessage): Unit = {
    if (hasPeer(peer)) {
      getPeerData(peer).get.setServiceIdentifier(versionMsg.services)
    } else {
      logger.warn(s"onVersionMessage called for unknown $peer")
    }
  }

  def buildPeerData(p: Peer, isPersistent: Boolean): PeerData = {
    val peerConnection = PeerConnection(p, queue)
    val peerMessageSender = PeerMessageSender(peerConnection)
    if (isPersistent) {
      PersistentPeerData(peer = p, peerMessageSender = peerMessageSender)
    } else {
      AttemptToConnectPeerData(p, peerMessageSender)
    }
  }

  /** Attempts to connect to various peers on the p2p network. Try to get more
    * peers for our node.
    */
  def queryForPeerConnections(excludePeers: Set[Peer]): Option[Unit] = {
    if (
      isConnectionSchedulerRunning.compareAndSet(false, true) && isStarted.get()
    ) {
      logger.debug(
        s"Attempting to find more peers to connect to... stack.size=${_peersToTry.size}"
      )
      val dbSlots = nodeAppConfig.maxConnectedPeers
      val dbPeersDbF =
        getLastSeenBlockFilterPeers(dbSlots)
      val dnsPeersF = if (_peersToTry.size < maxPeerSearchCount) {
        val pdsF = getPeersFromDnsSeeds
          .map { dnsPeers =>
            val shuffled = Random.shuffle(getPeersFromResources ++ dnsPeers)
            val pds = shuffled.map(p => buildPeerData(p, isPersistent = false))
            _peersToTry.pushAll(pds)
          }
          .map(_ => ())
        pdsF
      } else {
        Future.unit
      }
      val paramPdsF = for {
        _ <- dnsPeersF
        dbPeersDb <- dbPeersDbF
        dbPeers = dbPeersDb.map(_.peer(nodeAppConfig.socks5ProxyParams))
      } yield {
        val pds = paramPeers.map(buildPeerData(_, isPersistent = true))
        val dbPds = dbPeers.map(buildPeerData(_, isPersistent = false))
        _peersToTry.pushAll(pds ++ dbPds)
      }

      val peersToTryF = paramPdsF.map { _ =>
        // in case of less _peersToTry.size than maxPeerSearchCount
        val max = Math.min(maxPeerSearchCount, _peersToTry.size)
        val peers = (0
          .until(max)
          .map(_ => _peersToTry.pop()))
          .distinct
          .filterNot(p => excludePeers.exists(_ == p.peer))
        peers
      }
      val peersF = {
        for {
          peers <- peersToTryF
          _ = logger.debug(s"Trying next set of peers ${peers.map(_.peer)}")
          _ <- {
            Future.traverse(peers) { p =>
              // check if we already have an active connection
              val isDisconnectedF = peerManagerApi.isDisconnected(p.peer)
              for {
                isDisconnected <- isDisconnectedF
                _ <- {
                  if (isDisconnected) {
                    tryPeer(
                      peer = p.peer,
                      isPersistent = p.isInstanceOf[PersistentPeerData]
                    )
                  } else {
                    // do nothing, we are already connected
                    Future.unit
                  }
                }
              } yield ()
            }
          }
        } yield ()
      }
      peersF.onComplete {
        case Success(_) =>
          isConnectionSchedulerRunning.set(false)
        case Failure(_) =>
          isConnectionSchedulerRunning.set(false)
      }
      Some(())
    } else {
      logger.warn(
        s"Previous connection scheduler is still running or PeerFinder not started, skipping this run, it will run again in ${nodeAppConfig.tryNextPeersInterval}"
      )
      None
    }
  }

  override def toString: String = {
    s"PeerFinder(paramPeers=$paramPeers)"
  }
}

case class PeerOrdering(peerData: PeerData, priority: Int, id: Int)

case class PeerStack() {

  implicit private def ordering: Ordering[PeerOrdering] =
    (x: PeerOrdering, y: PeerOrdering) => {
      if (x.priority != y.priority) x.priority.compare(y.priority)
      else x.id.compare(y.id)
    }

  private var id: Int = 0

  private val maxSize = 5000

  private val set: mutable.SortedSet[PeerOrdering] =
    mutable.SortedSet[PeerOrdering]().empty

  def push(peer: PeerData, priority: Int = 0): Unit = {
    if (set.toVector.map(_.peerData.peer).contains(peer.peer)) {
      // noop, we already have this peer in our stack
      ()
    } else if (set.size == maxSize) {
      if (set.head.priority < priority) {
        set.remove(set.head)
        set.add(PeerOrdering(peer, priority, id))
        id += 1
      }
    } else {
      set.add(PeerOrdering(peer, priority, id))
      id += 1
    }
    ()
  }

  def pop(): PeerData = {
    val res = set.last.peerData
    set.remove(set.last)
    res
  }

  def size: Int = set.size

  def clear(): Unit = set.clear()

  def pushAll(peerDatas: Vector[PeerData], priority: Int = 0): Unit = {
    peerDatas.foreach(push(_, priority))
  }
}
