package org.bitcoins.node.networking.peer

import org.apache.pekko.{Done, NotUsed}
import org.apache.pekko.actor.{ActorSystem, Cancellable}
import org.apache.pekko.event.Logging
import org.apache.pekko.io.Inet.SocketOption
import org.apache.pekko.io.Tcp.SO.KeepAlive
import org.apache.pekko.stream.{Attributes, KillSwitches, UniqueKillSwitch}
import org.apache.pekko.stream.scaladsl.{
  BidiFlow,
  Flow,
  Keep,
  MergeHub,
  RunnableGraph,
  Sink,
  Source,
  SourceQueue,
  Tcp
}
import org.apache.pekko.util.ByteString
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.NodeStreamMessage.DisconnectedPeer
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.networking.peer.PeerConnection.ConnectionGraph
import org.bitcoins.node.{NodeStreamMessage, P2PLogger}
import org.bitcoins.tor.{Socks5Connection, Socks5ConnectionState}
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

case class PeerConnection(peer: Peer, queue: SourceQueue[NodeStreamMessage])(
    implicit
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig,
    system: ActorSystem
) extends P2PLogger {

  import system.dispatcher

  private val socket: InetSocketAddress = {
    nodeAppConfig.socks5ProxyParams match {
      case Some(proxy) => proxy.address
      case None        => peer.socket
    }
  }

  private val options: Vector[SocketOption] = Vector(KeepAlive(true))

  private var reconnectionTry = 0
  private var curReconnectionTry = 0
  private val reconnectionDelay = 500.millis
  private var reconnectionCancellableOpt: Option[Cancellable] = None

  private lazy val connection: Flow[
    ByteString,
    ByteString,
    Future[Tcp.OutgoingConnection]
  ] = {
    val base = Tcp(system)
      .outgoingConnection(
        remoteAddress = socket,
        halfClose = false,
        options = options
      )
    base
  }

  private val chainApi = ChainHandler.fromDatabase()

  private val versionMsgF: Future[NetworkMessage] = {
    chainApi.getBestHashBlockHeight().map { height =>
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg =
        VersionMessage(
          nodeAppConfig.network,
          NodeConstants.userAgent,
          Int32(height),
          InetAddress(localhost.getAddress),
          InetAddress(localhost.getAddress),
          nodeAppConfig.relay
        )
      NetworkMessage(nodeAppConfig.network, versionMsg)
    }
  }

  private def sendVersionMsg(): Future[Unit] = {
    versionMsgF.flatMap(v => sendMsg(v.bytes, mergeHubSink))
  }

  private def parseHelper(
      unalignedBytes: ByteString,
      byteVec: ByteString
  ): (ByteString, Vector[NetworkMessage]) = {
    val bytes: ByteVector = ByteVector(unalignedBytes ++ byteVec)
    logger.trace(s"Bytes for message parsing: ${bytes.toHex}")
    val (messages, newUnalignedBytes) =
      NetworkUtil.parseIndividualMessages(bytes)

    (ByteString.fromArray(newUnalignedBytes.toArray), messages)
  }

  private val parseToNetworkMsgFlow
      : Flow[ByteString, Vector[NetworkMessage], NotUsed] = {
    Flow[ByteString]
      .statefulMap(() => ByteString.empty)(
        parseHelper,
        { (_: ByteString) => None }
      )
      .log(
        "parseToNetworkMsgFlow",
        { case msgs: Vector[NetworkMessage] =>
          s"received msgs=${msgs.map(_.payload.commandName)} from peer=$peer"
        }
      )
      .withAttributes(Attributes.logLevels(onFailure = Logging.DebugLevel))
  }

  private val writeNetworkMsgFlow: Flow[ByteString, ByteString, NotUsed] = {
    Flow.apply
  }

  private val bidiFlow: BidiFlow[ByteString,
                                 Vector[
                                   NetworkMessage
                                 ],
                                 ByteString,
                                 ByteString,
                                 NotUsed] = {
    BidiFlow.fromFlows(parseToNetworkMsgFlow, writeNetworkMsgFlow)
  }

  private val (
    mergeHubSink: Sink[ByteString, NotUsed],
    mergeHubSource: Source[ByteString, NotUsed]
  ) = {
    MergeHub
      .source[ByteString](1024)
      .preMaterialize()
  }

  private val connectionFlow: Flow[ByteString,
                                   Vector[
                                     NetworkMessage
                                   ],
                                   Future[Tcp.OutgoingConnection]] =
    connection
      .idleTimeout(nodeAppConfig.peerTimeout)
      .joinMat(bidiFlow)(Keep.left)

  private def connectionGraph(
      handleNetworkMsgSink: Sink[Vector[NetworkMessage],
                                 (UniqueKillSwitch, Future[Done])]
  ): RunnableGraph[
    ((Future[Tcp.OutgoingConnection], UniqueKillSwitch), Future[Done])
  ] = {
    val result = mergeHubSource
      .viaMat(connectionFlow)(Keep.right)
      .toMat(handleNetworkMsgSink)(Keep.both)

    result.mapMaterializedValue(r => ((r._1, r._2._1), r._2._2))
  }

  private def buildConnectionGraph()
      : Future[((Tcp.OutgoingConnection, UniqueKillSwitch), Future[Done])] = {

    val handleNetworkMsgSink
        : Sink[Vector[NetworkMessage], (UniqueKillSwitch, Future[Done])] = {
      Flow[Vector[NetworkMessage]]
        .mapConcat(identity)
        .mapAsync(1) { case msg =>
          val wrapper = msg.payload match {
            case c: ControlPayload =>
              NodeStreamMessage.ControlMessageWrapper(c, peer)
            case d: DataPayload =>
              NodeStreamMessage.DataMessageWrapper(d, peer)
          }
          queue.offer(wrapper)
        }
        .viaMat(KillSwitches.single)(Keep.right)
        .toMat(Sink.ignore)(Keep.both)
    }

    val runningStream
        : Future[((Tcp.OutgoingConnection, UniqueKillSwitch), Future[Done])] = {
      nodeAppConfig.socks5ProxyParams match {
        case Some(s) =>
          val connectionSink =
            Flow[Either[ByteString, Socks5ConnectionState]]
              .mapAsync(1) {
                case Left(bytes) => Future.successful(bytes)
                case Right(state) =>
                  state match {
                    case Socks5ConnectionState.Connected =>
                      // need to send version message when we are first
                      // connected to initiate bitcoin protocol handshake
                      sendVersionMsg().map(_ => ByteString.empty)
                    case Socks5ConnectionState.Disconnected |
                        Socks5ConnectionState.Authenticating |
                        Socks5ConnectionState.Greeted =>
                      Future.successful(ByteString.empty)
                  }
              }
              .viaMat(parseToNetworkMsgFlow)(Keep.left)
              .toMat(handleNetworkMsgSink)(Keep.right)

          val source: Source[
            ByteString,
            Future[Tcp.OutgoingConnection]
          ] =
            mergeHubSource.viaMat(connection)(Keep.right)
          Socks5Connection
            .socks5Handler(
              socket = peer.socket,
              source = source,
              sink = connectionSink,
              mergeHubSink = mergeHubSink,
              credentialsOpt = s.credentialsOpt
            )
            .map(r => ((r._1, r._2._1), r._2._2))

        case None =>
          val result = connectionGraph(handleNetworkMsgSink).run()
          result._1._1.map(conn => ((conn, result._1._2), result._2))
      }
    }

    runningStream
  }

  def getLocalAddress: Future[Option[InetSocketAddress]] = {
    connectionGraphOpt match {
      case Some(g) => g.connectionF.map(c => Some(c.localAddress))
      case None    => Future.successful(None)
    }
  }

  @volatile private var connectionGraphOpt: Option[ConnectionGraph] = None

  /** Initiates a connection with the given peer */
  def connect(): Future[Unit] = {
    connectionGraphOpt match {
      case Some(_) =>
        logger.warn(s"Connected already to peer=${peer}")
        Future.unit
      case None =>
        logger.debug(s"Attempting to connect to peer=${peer}")

        val outgoingConnectionF = {
          buildConnectionGraph()
        }

        outgoingConnectionF.onComplete {
          case scala.util.Success(o) =>
            val tcp = o._1._1
            logger.debug(
              s"Connected to remote=${tcp.remoteAddress}  local=${tcp.localAddress}"
            )
          case scala.util.Failure(err) =>
            logger.debug(
              s"Failed to connect to peer=$peer with errMsg=${err.getMessage}"
            )
            val offerF =
              queue.offer(NodeStreamMessage.InitializationTimeout(peer))
            offerF.failed.foreach(err =>
              logger.error(
                s"Failed to offer initialize timeout for peer=$peer",
                err
              ))
        }

        val resultF: Future[Unit] = {
          for {
            outgoingConnection <- outgoingConnectionF
            graph = ConnectionGraph(
              mergeHubSink = mergeHubSink,
              connectionF = outgoingConnectionF.map(_._1._1),
              streamDoneF = outgoingConnection._2,
              killswitch = outgoingConnection._1._2
            )
            _ = {
              connectionGraphOpt = Some(graph)
              val _ = graph.streamDoneF
                .onComplete {
                  case scala.util.Success(_) =>
                    handleStreamComplete()
                  case scala.util.Failure(err) =>
                    logger.debug(
                      s"Connection with peer=$peer failed with err=${err.getMessage}"
                    )
                    handleStreamComplete()
                }
            }
            _ = resetReconnect()
            _ <- {
              nodeAppConfig.socks5ProxyParams match {
                case Some(_) => Future.unit
                case None    => sendVersionMsg()
              }
            }
          } yield ()
        }
        resultF.map(_ => ())
    }
  }

  private def handleStreamComplete(): Future[Unit] = {
    val disconnectedPeer = DisconnectedPeer(peer, false)
    val offerF = queue
      .offer(disconnectedPeer)
      .map(_ => ())
    offerF
  }

  /** resets reconnect state after connecting to a peer */
  private def resetReconnect(): Unit = {
    // cancel the job for reconnecting in case we were attempting to reconnect
    reconnectionCancellableOpt.map(_.cancel())
    reconnectionCancellableOpt = None
    reconnectionTry = 0
    curReconnectionTry = 0
  }

  def reconnect(): Future[Unit] = {
    connectionGraphOpt match {
      case Some(_) =>
        logger.error(
          s"Cannot reconnect when we have an active connection to peer=$peer"
        )
        Future.unit
      case None =>
        logger.info(s"Attempting to reconnect peer=$peer")
        val delay = reconnectionDelay * (1 << curReconnectionTry)
        curReconnectionTry += 1
        reconnectionTry = reconnectionTry + 1

        val reconnectP = Promise[Unit]()
        val cancellable = system.scheduler.scheduleOnce(delay) {
          val connF = connect()
          connF.onComplete {
            case scala.util.Success(_) =>
              resetReconnect()
              reconnectP.success(())
            case scala.util.Failure(exception) =>
              logger.error(s"Failed to reconnect with peer=$peer", exception)
              reconnectP.failure(exception)
          }
        }
        reconnectionCancellableOpt = Some(cancellable)
        reconnectP.future
    }
  }

  def isConnected(): Future[Boolean] = {
    Future.successful(connectionGraphOpt.isDefined)
  }

  def isDisconnected(): Future[Boolean] = {
    isConnected().map(!_)
  }

  /** Disconnects the given peer */
  def disconnect(): Future[Done] = {
    connectionGraphOpt match {
      case Some(cg) =>
        logger.debug(s"Disconnecting peer=${peer}")
        connectionGraphOpt = None
        cg.stop()
      case None =>
        Future.successful(Done)
    }
  }

  private[node] def sendMsg(msg: NetworkPayload): Future[Unit] = {
    // version or verack messages are the only messages that
    // can be sent before we are fully initialized
    // as they are needed to complete our handshake with our peer
    val networkMsg = NetworkMessage(nodeAppConfig.network, msg)
    sendMsg(networkMsg)
  }

  private[node] def sendMsg(msg: NetworkMessage): Future[Unit] = {
    logger.debug(
      s"Sending msg=${msg.header.commandName} to peer=${peer} socket=$socket"
    )
    connectionGraphOpt match {
      case Some(g) =>
        sendMsg(msg.bytes, g.mergeHubSink)
      case None =>
        val log =
          s"Could not send msg=${msg.payload.commandName} because we do not have an active connection to peer=${peer} socket=$socket"
        logger.warn(log)
        Future.unit
    }
  }

  private def sendMsg(
      bytes: ByteVector,
      mergeHubSink: Sink[ByteString, NotUsed]
  ): Future[Unit] = {
    sendMsg(ByteString.fromArray(bytes.toArray), mergeHubSink)
  }

  private def sendMsg(
      bytes: ByteString,
      mergeHubSink: Sink[ByteString, NotUsed]
  ): Future[Unit] = {
    val sendMsgF = Future {
      Source.single(bytes).to(mergeHubSink).run()
    }.map(_ => ())
    sendMsgF
  }
}

object PeerConnection {

  case class ConnectionGraph(
      mergeHubSink: Sink[ByteString, NotUsed],
      connectionF: Future[Tcp.OutgoingConnection],
      streamDoneF: Future[Done],
      killswitch: UniqueKillSwitch
  ) {

    def stop(): Future[Done] = {
      killswitch.shutdown()
      streamDoneF
    }
  }

}
