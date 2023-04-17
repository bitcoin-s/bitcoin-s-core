package org.bitcoins.node.networking.peer

import akka.actor.ActorSystem
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageReceiverState._
import org.bitcoins.node.{Node, P2PLogger}

import scala.concurrent.Future

/** Responsible for receiving messages from a peer on the
  * p2p network. This is called by [[org.bitcoins.rpc.client.common.Client Client]] when doing the p2p
  * handshake and during the [[PeerMessageReceiverState.Normal Normal]]
  * operations. This is the entry point for handling all received
  * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]]
  */
class PeerMessageReceiver(
    node: Node,
    val state: PeerMessageReceiverState,
    peer: Peer
)(implicit system: ActorSystem, nodeAppConfig: NodeAppConfig)
    extends P2PLogger {
  import system.dispatcher

  require(nodeAppConfig.nodeType != NodeType.BitcoindBackend,
          "Bitcoind should handle the P2P interactions")

  def stopReconnect(): PeerMessageReceiver = {
    state match {
      case Preconnection =>
        //when retry, state should be back to preconnection
        val newState = StoppedReconnect(state.clientConnectP,
                                        state.clientDisconnectP,
                                        state.versionMsgP,
                                        state.verackMsgP)
        val newRecv = toState(newState)
        newRecv
      case _: StoppedReconnect =>
        logger.warn(
          s"Already stopping reconnect from peer=$peer, this is a noop")
        this
      case bad @ (_: Initializing | _: Normal | _: InitializedDisconnect |
          _: InitializedDisconnectDone | _: Disconnected | _: Waiting) =>
        throw new RuntimeException(
          s"Cannot stop reconnect from peer=$peer when in state=$bad")
    }
  }

  private[networking] def isConnected: Boolean = state.isConnected

  private[networking] def isDisconnected: Boolean = state.isDisconnected

  private[networking] def hasReceivedVersionMsg: Boolean =
    state.hasReceivedVersionMsg.isCompleted

  private[networking] def hasReceivedVerackMsg: Boolean =
    state.hasReceivedVerackMsg.isCompleted

  private[networking] def isInitialized: Boolean = state.isInitialized

  def handleNetworkMessageReceived(
      networkMsgRecv: PeerMessageReceiver.NetworkMessageReceived): Future[
    PeerMessageReceiver] = {

    val client = networkMsgRecv.client

    //create a way to send a response if we need too
    val peerMsgSender = PeerMessageSender(client)

    logger.debug(
      s"Received message=${networkMsgRecv.msg.header.commandName} from peer=${client.peer} state=${state} ")

    val payload = networkMsgRecv.msg.payload

    //todo: this works but doesn't seem to be the best place to do this
    val curReceiver: PeerMessageReceiver = {
      state match {
        case state: Waiting =>
          val responseFor = state.responseFor.asInstanceOf[ExpectsResponse]
          if (responseFor.isPayloadExpectedResponse(payload)) {
            val timeTaken = System.currentTimeMillis() - state.waitingSince
            logger.debug(
              s"Received expected response ${payload.commandName} in $timeTaken ms")
            state.expectedResponseCancellable.cancel()
            val newState = Normal(state.clientConnectP,
                                  state.clientDisconnectP,
                                  state.versionMsgP,
                                  state.verackMsgP)
            toState(newState)
          } else this
        case state: Initializing =>
          if (payload == VerAckMessage)
            state.initializationTimeoutCancellable.cancel()
          this
        case _ => this
      }
    }

    networkMsgRecv.msg.payload match {
      case controlPayload: ControlPayload =>
        handleControlPayload(payload = controlPayload,
                             sender = peerMsgSender,
                             curReceiver)
      case dataPayload: DataPayload =>
        handleDataPayload(payload = dataPayload,
                          sender = peerMsgSender,
                          curReceiver)
    }
  }

  /** Handles a [[DataPayload]] message. It checks if the sender is the parent
    * actor, it sends it to our peer on the network. If the sender was the
    * peer on the network, forward to the actor that spawned our actor
    *
    * @param payload
    * @param sender
    */
  private def handleDataPayload(
      payload: DataPayload,
      sender: PeerMessageSender,
      curReceiver: PeerMessageReceiver): Future[PeerMessageReceiver] = {
    //else it means we are receiving this data payload from a peer,
    //we need to handle it
    node.getDataMessageHandler
      .addToStream(payload, sender, peer)
      .map(_ => new PeerMessageReceiver(node, curReceiver.state, peer))
  }

  /** Handles control payloads defined here https://bitcoin.org/en/developer-reference#control-messages
    *
    * @param payload the payload we need to do something with
    * @param sender the [[PeerMessageSender]] we can use to initialize an subsequent messages that need to be sent
    * @return the requests with the request removed for which the @payload is responding too
    */
  private def handleControlPayload(
      payload: ControlPayload,
      sender: PeerMessageSender,
      curReceiver: PeerMessageReceiver): Future[PeerMessageReceiver] = {
    node.controlMessageHandler
      .handleControlPayload(payload, sender, peer, curReceiver)
  }

  /** Transitions our PeerMessageReceiver to a new state */
  def toState(newState: PeerMessageReceiverState): PeerMessageReceiver = {
    new PeerMessageReceiver(
      node = node,
      state = newState,
      peer = peer
    )
  }
}

object PeerMessageReceiver {

  sealed abstract class PeerMessageReceiverMsg {

    /** Who we need to use to send a reply to our peer
      * if a response is needed for this message
      */
    def client: P2PClient
  }

  case class NetworkMessageReceived(msg: NetworkMessage, client: P2PClient)
      extends PeerMessageReceiverMsg

  def apply(state: PeerMessageReceiverState, node: Node, peer: Peer)(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig
  ): PeerMessageReceiver = {
    new PeerMessageReceiver(node = node, state = state, peer = peer)
  }

  /** Creates a peer message receiver that is ready
    * to be connected to a peer. This can be given to [[org.bitcoins.node.networking.P2PClient.props() P2PClient]]
    * to connect to a peer on the network
    */
  def preConnection(peer: Peer, node: Node)(implicit
      system: ActorSystem,
      nodeAppConfig: NodeAppConfig
  ): PeerMessageReceiver = {
    PeerMessageReceiver(node = node,
                        state = PeerMessageReceiverState.fresh(),
                        peer = peer)
  }

  def newReceiver(node: Node, peer: Peer)(implicit
      nodeAppConfig: NodeAppConfig,
      system: ActorSystem): PeerMessageReceiver = {
    PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                        node = node,
                        peer = peer)
  }
}
