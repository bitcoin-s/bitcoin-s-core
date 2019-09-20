package org.bitcoins.node.networking.peer

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.p2p.{DataPayload, HeadersMessage, InventoryMessage}

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.p2p.BlockMessage
import org.bitcoins.core.p2p.TransactionMessage
import org.bitcoins.core.p2p.MerkleBlockMessage
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.SpvNodeCallbacks
import org.bitcoins.core.p2p.GetDataMessage
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import slick.jdbc.SQLiteProfile
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.core.p2p.TypeIdentifier
import org.bitcoins.core.p2p.MsgUnassigned
import org.bitcoins.core.p2p.Inventory
import org.bitcoins.core.p2p.CompactFilterCheckPointMessage
import org.bitcoins.core.p2p.CompactFilterHeadersMessage
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.p2p.GetBlocksMessage
import org.bitcoins.core.p2p.MemPoolMessage
import org.bitcoins.core.p2p.GetHeadersMessage
import org.bitcoins.core.p2p.GetCompactFiltersMessage
import org.bitcoins.core.p2p.GetCompactFilterHeadersMessage
import org.bitcoins.core.p2p.GetCompactFilterCheckPointMessage
import org.bitcoins.core.util.FutureUtil

/** This actor is meant to handle a [[org.bitcoins.core.p2p.DataPayload DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[org.bitcoins.core.p2p.HeadersMessage HeadersMessage]] we should store those headers in our database
  */
case class DataMessageHandler(
                               chainApi: ChainApi,
                               callbacks: SpvNodeCallbacks,
                               receivedFilterCount: Int = 0,
                               filtersSyncing: Boolean = false)(
    implicit ec: ExecutionContext,
    appConfig: NodeAppConfig,
    chainConfig: ChainAppConfig)
    extends P2PLogger {

  private val txDAO = BroadcastAbleTransactionDAO(SQLiteProfile)

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {

    payload match {
      case checkpoint: CompactFilterCheckPointMessage =>
        logger.debug(
          s"Got ${checkpoint.filterHeaders.size} checkpoints ${checkpoint}")
        for {
          newChainApi <- chainApi.processCheckpoints(
            checkpoint.filterHeaders.map(_.flip),
            checkpoint.stopHash.flip)
        } yield {
          this.copy(chainApi = newChainApi)
        }
      case filterHeader: CompactFilterHeadersMessage =>
        logger.info(
          s"Got ${filterHeader.filterHashes.size} compact filter header hashes")
        val filterHeaders = filterHeader.filterHeaders
        for {
          newChainApi <- chainApi.processFilterHeaders(
            filterHeaders,
            filterHeader.stopHash.flip)
          newFiltersSyncing <- if (filterHeaders.size == chainConfig.maxFilterHeaderCount) {
            logger.info(
              s"Received maximum amount of filter headers in one header message. This means we are not synced, requesting more")
            sendNextGetCompactFilterHeadersCommand(peerMsgSender,
                                                   filterHeader.stopHash.flip).map(_ => filtersSyncing)
          } else {
            logger.debug(
              s"Received filter headers=${filterHeaders.size} in one message, " +
                "which is less than max. This means we are synced.")
            sendFirstGetCompactFilterCommand(peerMsgSender).map { synced =>
              if (!synced)
                logger.info("We are synced")
              synced
            }
          }
        } yield {
          this.copy(chainApi = newChainApi, filtersSyncing = newFiltersSyncing)
        }
      case filter: CompactFilterMessage =>
        logger.debug(s"Received ${filter.commandName}, $filter")
        for {
          (newCount, newFiltersSyncing) <- if (receivedFilterCount == chainConfig.maxFilterCount - 1) {
            logger.info(
              s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
            for {
              _ <- sendNextGetCompactFilterCommand(peerMsgSender,
                                                   filter.blockHash.flip)
            } yield (0, filtersSyncing)
          } else {
            for {
              filterHeaderCount <- chainApi.getFilterHeaderCount
              filterCount <- chainApi.getFilterCount
            } yield {
              val syncing = filterCount < filterHeaderCount - 1
              if (!syncing) {
                logger.info(s"We are synced")
              }
              (receivedFilterCount + 1, syncing)
            }
          }
          newChainApi <- chainApi.processFilter(filter)
        } yield {
          this.copy(chainApi = newChainApi, receivedFilterCount = newCount, filtersSyncing = newFiltersSyncing)
        }
      case notHandling @ (MemPoolMessage | _: GetHeadersMessage |
          _: GetBlocksMessage | _: GetCompactFiltersMessage |
          _: GetCompactFilterHeadersMessage |
          _: GetCompactFilterCheckPointMessage) =>
        logger.debug(s"Received ${notHandling.commandName} message, skipping ")
        Future.successful(this)
      case getData: GetDataMessage =>
        logger.info(
          s"Received a getdata message for inventories=${getData.inventories}")
        getData.inventories.foreach { inv =>
          logger.debug(s"Looking for inv=$inv")
          inv.typeIdentifier match {
            case TypeIdentifier.MsgTx =>
              txDAO.findByHash(inv.hash).map {
                case Some(tx) =>
                  peerMsgSender.sendTransactionMessage(tx.transaction)
                case None =>
                  logger.warn(
                    s"Got request to send data with hash=${inv.hash}, but found nothing")
              }
            case other @ (TypeIdentifier.MsgBlock |
                TypeIdentifier.MsgFilteredBlock |
                TypeIdentifier.MsgCompactBlock |
                TypeIdentifier.MsgFilteredWitnessBlock |
                TypeIdentifier.MsgWitnessBlock | TypeIdentifier.MsgWitnessTx) =>
              logger.warn(
                s"Got request to send data type=$other, this is not implemented yet")

            case unassigned: MsgUnassigned =>
              logger.warn(
                s"Received unassigned message we do not understand, msg=${unassigned}")
          }

        }
        Future.successful(this)
      case HeadersMessage(count, headers) =>
        logger.info(s"Received headers message with ${count.toInt} headers")
        logger.trace(
          s"Received headers=${headers.map(_.hashBE.hex).mkString("[", ",", "]")}")
        val chainApiF = chainApi.processHeaders(headers)

        if (appConfig.isSPVEnabled) {
          logger.trace(s"Requesting data for headers=${headers.length}")
          peerMsgSender.sendGetDataMessage(headers: _*)
        }

        val getHeadersF = chainApiF
          .flatMap { newApi =>
            if (headers.nonEmpty) {

              val lastHeader = headers.last
              val lastHash = lastHeader.hash
              newApi.getBlockCount.map { count =>
                logger.trace(
                  s"Processed headers, most recent has height=$count and hash=$lastHash.")
              }

              if (count.toInt == HeadersMessage.MaxHeadersCount) {
                logger.error(
                  s"Received maximum amount of headers in one header message. This means we are not synced, requesting more")
                peerMsgSender.sendGetHeadersMessage(lastHash).map(_ =>  filtersSyncing)
              } else {
                logger.debug(
                  List(s"Received headers=${count.toInt} in one message,",
                       "which is less than max. This means we are synced,",
                       "not requesting more.")
                    .mkString(" "))
                if (appConfig.isNeutrinoEnabled && !filtersSyncing)
                  sendFirstGetCompactFilterHeadersCommand(peerMsgSender)
                else
                  Future.successful(filtersSyncing)
              }
            }
            else
              Future.successful(filtersSyncing)
          }

        getHeadersF.failed.map { err =>
          logger.error(s"Error when processing headers message", err)
        }

        for {
          newApi <- chainApiF
          newFiltersSyncing <- getHeadersF
        } yield {
          logger.info(s"HeadersMessage ${newFiltersSyncing}")
          this.copy(chainApi = newApi, filtersSyncing = newFiltersSyncing)
        }
      case msg: BlockMessage =>
        Future {
          callbacks.onBlockReceived.foreach(_.apply(msg.block))
          this
        }
      case TransactionMessage(tx) =>
        val belongsToMerkle =
          MerkleBuffers.putTx(tx, callbacks.onMerkleBlockReceived)
        if (belongsToMerkle) {
          logger.trace(
            s"Transaction=${tx.txIdBE} belongs to merkleblock, not calling callbacks")
          Future.successful(this)
        } else {
          logger.trace(
            s"Transaction=${tx.txIdBE} does not belong to merkleblock, processing given callbacks")
          Future {
            callbacks.onTxReceived.foreach(_.apply(tx))
            this
          }
        }
      case MerkleBlockMessage(merkleBlock) =>
        MerkleBuffers.putMerkle(merkleBlock)
        Future.successful(this)
      case invMsg: InventoryMessage =>
        handleInventoryMsg(invMsg = invMsg, peerMsgSender = peerMsgSender)
    }
  }

  private def sendNextGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender,
      stopHash: DoubleSha256DigestBE): Future[Boolean] =
    sendNextBatchCommand(stopHash,
                         chainConfig.maxFilterHeaderCount,
                         "compact filter headers")(
      peerMsgSender.sendGetCompactFilterHeadersMessage)

  private def sendFirstGetCompactFilterHeadersCommand(
      peerMsgSender: PeerMessageSender): Future[Boolean] =
    for {
      filterHeaderCount <- chainApi.getFilterHeaderCount
      highestFilterHeaderOpt <- chainApi.getFilterHeadersAtHeight(filterHeaderCount)
        .map(_.headOption)
      highestFilterBlockHash = highestFilterHeaderOpt
        .map(_.blockHashBE)
        .getOrElse(DoubleSha256DigestBE.empty)
      res <- sendNextGetCompactFilterHeadersCommand(peerMsgSender,
                                                    highestFilterBlockHash)
    } yield res

  private def sendNextGetCompactFilterCommand(
      peerMsgSender: PeerMessageSender,
      stopHash: DoubleSha256DigestBE): Future[Boolean] =
    sendNextBatchCommand(
      stopHash,
      chainConfig.maxFilterCount,
      "compact filters")(peerMsgSender.sendGetCompactFiltersMessage)

  private def sendFirstGetCompactFilterCommand(
      peerMsgSender: PeerMessageSender): Future[Boolean] =
    for {
      filterCount <- chainApi.getFilterCount
      highestFilterOpt <- chainApi.getFiltersAtHeight(filterCount).map(_.headOption)
      highestFilterBlockHash = highestFilterOpt
        .map(_.blockHashBE)
        .getOrElse(DoubleSha256DigestBE.empty)
      res <- sendNextGetCompactFilterCommand(peerMsgSender,
                                             highestFilterBlockHash)
    } yield res

  private def sendNextBatchCommand(
      stopHash: DoubleSha256DigestBE,
      batchSize: Int,
      message: String)(
      f: (Int, DoubleSha256Digest) => Future[Unit]): Future[Boolean] = {
    for {
      nextRangeOpt <- chainApi.nextBatchRange(stopHash, batchSize)
      res <- nextRangeOpt match {
        case Some((startHeight, stopHash)) =>
          logger.info(s"Requesting $message from=$startHeight to=$stopHash")
          f(startHeight, stopHash).map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peerMsgSender: PeerMessageSender): Future[DataMessageHandler] = {
    logger.info(s"Received inv=${invMsg}")
    val getData = GetDataMessage(invMsg.inventories.map {
      case Inventory(TypeIdentifier.MsgBlock, hash) =>
        Inventory(TypeIdentifier.MsgFilteredBlock, hash)
      case other: Inventory => other
    })
    peerMsgSender.sendMsg(getData)
    Future.successful(this)

  }
}

object DataMessageHandler {

  /** Callback for handling a received block */
  type OnBlockReceived = Block => Unit

  /** Callback for handling a received Merkle block with its corresponding TXs */
  type OnMerkleBlockReceived = (MerkleBlock, Vector[Transaction]) => Unit

  /** Callback for handling a received transaction */
  type OnTxReceived = Transaction => Unit

  /** Does nothing */
  def noop[T]: T => Unit = _ => ()

}
