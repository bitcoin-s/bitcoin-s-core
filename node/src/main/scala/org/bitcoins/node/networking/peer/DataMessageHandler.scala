package org.bitcoins.node.networking.peer

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.bitcoins.core.p2p.{DataPayload, HeadersMessage, InventoryMessage}

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.p2p.BlockMessage
import org.bitcoins.core.p2p.TransactionMessage
import org.bitcoins.core.p2p.MerkleBlockMessage
import org.bitcoins.node.SpvNodeCallbacks

/** This actor is meant to handle a [[org.bitcoins.node.messages.DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[HeadersMessage]] we should store those headers in our database
  */
class DataMessageHandler(callbacks: Vector[SpvNodeCallbacks])(
    implicit ec: ExecutionContext,
    appConfig: ChainAppConfig)
    extends BitcoinSLogger {

  private val blockHeaderDAO: BlockHeaderDAO = BlockHeaderDAO()

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[Unit] = {
    payload match {
      case headersMsg: HeadersMessage =>
        val headers = headersMsg.headers
        val chainApi: ChainApi =
          ChainHandler(blockHeaderDAO, chainConfig = appConfig)
        val chainApiF = chainApi.processHeaders(headers)

        chainApiF.map { _ =>
          val lastHash = headers.last.hash
          peerMsgSender.sendGetHeadersMessage(lastHash)
        }
      case msg: BlockMessage =>
        callbacks.foreach(_.onBlockReceived(msg.block))
        FutureUtil.unit
      case msg: TransactionMessage =>
        callbacks.foreach(_.onTxReceived(msg.transaction))
        FutureUtil.unit
      case msg: MerkleBlockMessage =>
        callbacks.foreach(_.onMerkleBlockReceived(msg.merkleBlock))
        FutureUtil.unit
      case invMsg: InventoryMessage =>
        handleInventoryMsg(invMsg = invMsg, peerMsgSender = peerMsgSender)
    }
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peerMsgSender: PeerMessageSender): Future[Unit] = {
    logger.info(s"Received inv=${invMsg}")

    FutureUtil.unit

  }
}

object DataMessageHandler {

  /** Callback for handling a received block */
  type OnBlockReceived = Block => Unit

  /** Does nothing with the received block */
  val noopBlockReceived: OnBlockReceived = _ => ()

  /** Callback for handling a received Merkle block */
  type OnMerkleBlockReceived = MerkleBlock => Unit

  /** Does nothing with the received Merkle block */
  val noopMerkleBlockReceived: OnMerkleBlockReceived = _ => ()

  /** Callback for handling a received transaction */
  type OnTxReceived = Transaction => Unit

  /** Does nothing with the received transaction */
  val noopTxReceived: OnTxReceived = _ => ()

}
