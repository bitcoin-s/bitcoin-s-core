package org.bitcoins.node.networking.peer

import akka.stream.scaladsl.SourceQueue
import org.bitcoins.chain.blockchain.{DuplicateHeaders, InvalidBlockHeader}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.CompactFilterHeaderDb
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.gcs.{BlockFilter, GolombFilter}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._
import org.bitcoins.node.networking.peer.NodeState._
import org.bitcoins.node.util.PeerMessageSenderApi
import org.bitcoins.node.{NodeStreamMessage, P2PLogger, PeerData, PeerManager}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** This actor is meant to handle a [[org.bitcoins.core.p2p.DataPayload DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[org.bitcoins.core.p2p.HeadersMessage HeadersMessage]] we should store those headers in our database
  *
  * @param currentFilterBatch holds the current batch of filters to be processed, after its size reaches
  *                           chainConfig.filterBatchSize they will be processed and then emptied
  */
case class DataMessageHandler(
    chainApi: ChainApi,
    walletCreationTimeOpt: Option[Instant],
    queue: SourceQueue[NodeStreamMessage],
    peerMessageSenderApi: PeerMessageSenderApi,
    state: NodeState,
    filterBatchCache: Set[CompactFilterMessage])(implicit
    ec: ExecutionContext,
    appConfig: NodeAppConfig,
    chainConfig: ChainAppConfig)
    extends P2PLogger {

  require(appConfig.nodeType == NodeType.NeutrinoNode,
          "DataMessageHandler is meant to be used with NeutrinoNode")

  private val txDAO = BroadcastAbleTransactionDAO()

  private val syncing: Boolean = state.isSyncing

  private def isChainIBD: Future[Boolean] = {
    chainApi.isIBD()
  }

  def handleDataPayload(
      payload: DataPayload,
      peerData: PeerData): Future[DataMessageHandler] = {
    state match {
      case syncState: SyncNodeState =>
        syncState match {
          case _: ValidatingHeaders =>
            val resultF =
              handleDataPayloadValidState(payload, peerData)
            //process messages from all peers
            resultF.failed.foreach { err =>
              logger.error(
                s"Failed to handle data payload=${payload} from peer=${peerData.peer} in state=$state errMsg=${err.getMessage}",
                err)
            }
            resultF.recoverWith { case NonFatal(_) =>
              Future.successful(this)
            }
          case state @ (_: HeaderSync | _: FilterHeaderSync | _: FilterSync) =>
            val syncPeer = state.syncPeer
            if (peerData.peer != syncPeer) {
              //ignore message from peers that we aren't syncing with during IBD
              logger.warn(
                s"Ignoring message ${payload.commandName} from peer=${peerData.peer} in state=$state because we are syncing with this peer currently. syncPeer=$syncPeer")
              Future.successful(this)
            } else {
              val resultF =
                handleDataPayloadValidState(payload, peerData)
              resultF.failed.foreach { err =>
                logger.error(
                  s"Failed to handle data payload=${payload} from peer=${peerData.peer} in state=$state errMsg=${err.getMessage}",
                  err)
              }
              resultF.recoverWith { case NonFatal(_) =>
                Future.successful(this)
              }
            }
        }
      case DoneSyncing(_) =>
        val resultF = handleDataPayloadValidState(payload, peerData)

        resultF.failed.foreach { err =>
          logger.error(
            s"Failed to handle data payload=${payload} from peer=${peerData.peer} in state=$state errMsg=${err.getMessage}",
            err)
        }

        resultF.recoverWith { case NonFatal(_) =>
          Future.successful(this)
        }

      case MisbehavingPeer(badPeer, peers) =>
        if (badPeer == peerData.peer) {
          Future.failed(
            new RuntimeException(
              s"Cannot continue processing p2p messages from badPeer=$badPeer"))
        } else {
          //re-review this, we should probably pattern match on old state so we can continue syncing
          //from where we left off?
          copy(state = DoneSyncing(peers)).handleDataPayload(payload, peerData)
        }
      case RemovePeers(badPeers, peers, _) =>
        if (badPeers.exists(_ == peerData.peer)) {
          Future.failed(new RuntimeException(
            s"Cannot continue processing p2p messages from peer we were suppose to remove, peer=${peerData.peer}"))
        } else {
          copy(state = DoneSyncing(peers)).handleDataPayload(payload, peerData)
        }

    }

  }

  /** Processes a [[DataPayload]] if our [[NodeState]] is valid.
    * We ignore messages from certain peers when we are in initial block download.
    */
  private def handleDataPayloadValidState(
      payload: DataPayload,
      peerData: PeerData): Future[DataMessageHandler] = {
    val peer = peerData.peer
    val wrappedFuture: Future[Future[DataMessageHandler]] = Future {
      payload match {
        case checkpoint: CompactFilterCheckPointMessage =>
          logger.debug(
            s"Got ${checkpoint.filterHeaders.size} checkpoints ${checkpoint} from $peer")
          for {
            newChainApi <- chainApi.processCheckpoints(
              checkpoint.filterHeaders.map(_.flip),
              checkpoint.stopHash.flip)
          } yield {
            this.copy(chainApi = newChainApi)
          }
        case filterHeader: CompactFilterHeadersMessage =>
          logger.debug(
            s"Got ${filterHeader.filterHashes.size} compact filter header hashes, state=$state")
          val filterHeaderSync = state match {
            //is validating headers right here? Re-review these state transitions
            case s @ (_: HeaderSync | _: ValidatingHeaders | DoneSyncing(_)) =>
              FilterHeaderSync(peer, s.peers)
            case filterHeaderSync: FilterHeaderSync => filterHeaderSync
            case x @ (_: FilterSync | _: MisbehavingPeer | _: RemovePeers) =>
              sys.error(
                s"Incorrect state for handling filter header messages, got=$x")
          }
          val filterHeaders = filterHeader.filterHeaders
          for {
            newChainApi <- chainApi.processFilterHeaders(
              filterHeaders,
              filterHeader.stopHash.flip)
            newState <-
              if (filterHeaders.size == chainConfig.filterHeaderBatchSize) {
                logger.debug(
                  s"Received maximum amount of filter headers in one header message. This means we are not synced, requesting more")
                sendNextGetCompactFilterHeadersCommand(
                  peerMessageSenderApi = peerMessageSenderApi,
                  syncPeer = peer,
                  prevStopHash = filterHeader.stopHash.flip).map(_ =>
                  filterHeaderSync)
              } else {
                for {
                  startHeightOpt <- getCompactFilterStartHeight(
                    walletCreationTimeOpt)
                  filterSyncStateOpt <- sendFirstGetCompactFilterCommand(
                    peerMessageSenderApi = peerMessageSenderApi,
                    syncPeer = peer,
                    startHeightOpt = startHeightOpt,
                    peers = filterHeaderSync.peers)
                } yield {
                  filterSyncStateOpt match {
                    case Some(filterSyncState) => filterSyncState
                    case None                  => DoneSyncing(filterHeaderSync.peers)
                  }
                }
              }
            newChainApi <- newChainApi.setSyncing(newState.isSyncing)
          } yield {
            this.copy(chainApi = newChainApi, state = newState)
          }
        case filter: CompactFilterMessage =>
          logger.debug(
            s"Received ${filter.commandName}, filter.blockHash=${filter.blockHash.flip} state=$state")
          val filterSyncState = state match {
            case f: FilterSync => f
            case s @ (_: DoneSyncing | _: FilterHeaderSync) =>
              FilterSync(peer, s.peers)
            case x @ (_: MisbehavingPeer | _: RemovePeers | _: HeaderSync |
                _: ValidatingHeaders) =>
              sys.error(s"Incorrect state for handling filter messages, got=$x")
          }
          val filterBatch = filterBatchCache.+(filter)
          val batchSizeFull: Boolean =
            filterBatch.size == chainConfig.filterBatchSize
          for {
            isFiltersSynced <- isFiltersSynced(chainApi, filterBatch)
            // If we are not syncing or our filter batch is full, process the filters
            (newBatch: Set[CompactFilterMessage], newChainApi) <- {
              if (isFiltersSynced || batchSizeFull) {

                logger.info(s"Processing ${filterBatch.size} filters")
                val sortedBlockFiltersF =
                  sortBlockFiltersByBlockHeight(filterBatch)
                for {
                  sortedBlockFilters <- sortedBlockFiltersF
                  sortedFilterMessages = sortedBlockFilters.map(_._2)
                  newChainApi <- chainApi.processFilters(sortedFilterMessages)
                  sortedGolombFilters = sortedBlockFilters.map(x =>
                    (x._1, x._3))
                  _ <-
                    appConfig.callBacks
                      .executeOnCompactFiltersReceivedCallbacks(
                        sortedGolombFilters)
                } yield (Set.empty, newChainApi)
              } else Future.successful((filterBatch, chainApi))
            }
            (_, newFilterHeight) <-
              calcFilterHeaderFilterHeight(chainApi)
            filterHeaderSyncStateOpt <-
              if (batchSizeFull) {
                logger.info(
                  s"Received maximum amount of filters in one batch. This means we are not synced, requesting more")
                sendNextGetCompactFilterCommand(peerMessageSenderApi,
                                                peer,
                                                newFilterHeight,
                                                filterSyncState.peers)
              } else Future.successful(Some(filterSyncState))
            newDmhState <- {
              if (isFiltersSynced) {
                syncIfHeadersAhead(peer, filterSyncState.peers)
              } else {
                val res = filterHeaderSyncStateOpt match {
                  case Some(filterSyncState) => filterSyncState
                  case None                  => DoneSyncing(filterSyncState.peers)
                }
                Future.successful(res)
              }
            }
            newChainApi <- newChainApi.setSyncing(newDmhState.isSyncing)
            _ <- checkIBD(newChainApi)
          } yield {
            this.copy(
              chainApi = newChainApi,
              filterBatchCache = newBatch,
              state = newDmhState
            )
          }
        case notHandling @ (MemPoolMessage | _: GetHeadersMessage |
            _: GetBlocksMessage | _: GetCompactFiltersMessage |
            _: GetCompactFilterHeadersMessage |
            _: GetCompactFilterCheckPointMessage) =>
          logger.debug(
            s"Received ${notHandling.commandName} message, skipping ")
          Future.successful(this)
        case getData: GetDataMessage =>
          logger.info(
            s"Received a getdata message for inventories=${getData.inventories}")
          getData.inventories.foreach { inv =>
            logger.debug(s"Looking for inv=$inv")
            inv.typeIdentifier match {
              case msgTx @ (TypeIdentifier.MsgTx |
                  TypeIdentifier.MsgWitnessTx) =>
                txDAO.findByHash(inv.hash).flatMap {
                  case Some(BroadcastAbleTransaction(tx)) =>
                    val txToBroadcast =
                      if (msgTx == TypeIdentifier.MsgTx) {
                        // send non-witness serialization
                        tx.toBaseTx
                      } else tx // send normal serialization

                    peerMessageSenderApi.sendTransactionMessage(transaction =
                                                                  txToBroadcast,
                                                                peerOpt =
                                                                  Some(peer))
                  case None =>
                    logger.warn(
                      s"Got request to send data with hash=${inv.hash}, but found nothing")
                    Future.unit
                }
              case other @ (TypeIdentifier.MsgBlock |
                  TypeIdentifier.MsgFilteredBlock |
                  TypeIdentifier.MsgCompactBlock |
                  TypeIdentifier.MsgFilteredWitnessBlock |
                  TypeIdentifier.MsgWitnessBlock) =>
                logger.warn(
                  s"Got request to send data type=$other, this is not implemented yet")

              case unassigned: MsgUnassigned =>
                logger.warn(
                  s"Received unassigned message we do not understand, msg=${unassigned}")
            }

          }
          Future.successful(this)
        case HeadersMessage(count, headers) =>
          logger.info(
            s"Received headers message with ${count.toInt} headers from peer=$peer state=$state")
          val headerSyncState = state match {
            case DoneSyncing(peers) =>
              if (count.toInt != 0) {
                //why do we sometimes get empty HeadersMessage?
                HeaderSync(peer, peers)
              } else DoneSyncing(peers)
            case headerSync: HeaderSync =>
              if (headerSync.syncPeer == peer) {
                headerSync
              } else {
                //are these Set.empty correct?
                ValidatingHeaders(syncPeer = peer,
                                  inSyncWith = Set.empty,
                                  failedCheck = Set.empty,
                                  verifyingWith = headerSync.peers,
                                  peers = headerSync.peers)
              }
            case v: ValidatingHeaders => v
            case x @ (_: FilterHeaderSync | _: FilterSync | _: MisbehavingPeer |
                _: RemovePeers) =>
              sys.error(s"Invalid state to receive headers in, got=$x")
          }
          val chainApiHeaderProcessF: Future[DataMessageHandler] = for {
            newChainApi <- chainApi.setSyncing(count.toInt > 0)
            processed <- newChainApi.processHeaders(headers)
          } yield {
            copy(state = headerSyncState, chainApi = processed)
          }

          val getHeadersF: Future[DataMessageHandler] = {
            for {
              newDmh <- chainApiHeaderProcessF
              dmh <- getHeaders(newDmh = newDmh, headers = headers, peer = peer)
            } yield dmh
          }
          val recoveredDmhF = getHeadersF.recoverWith {
            case _: DuplicateHeaders =>
              logger.warn(
                s"Received duplicate headers from ${peer} in state=$state")
              Future.successful(this)
            case _: InvalidBlockHeader =>
              logger.warn(
                s"Invalid headers of count $count sent from ${peer} in state=$state")
              recoverInvalidHeader(peerData)
            case e: Throwable => throw e
          }

          recoveredDmhF.failed.map { err =>
            logger.error(
              s"Error when processing headers message: ${err.getMessage}")
          }

          for {
            newDmh <- recoveredDmhF
            _ <- appConfig.callBacks.executeOnBlockHeadersReceivedCallbacks(
              headers)
          } yield {
            newDmh
          }
        case msg: BlockMessage =>
          val block = msg.block
          logger.info(
            s"Received block message with hash ${block.blockHeader.hash.flip.hex}")

          val newMsgHandlerF = {
            chainApi
              .getHeader(block.blockHeader.hashBE)
              .flatMap { headerOpt =>
                if (headerOpt.isEmpty) {
                  logger.debug("Processing block's header...")
                  val headersMessage =
                    HeadersMessage(CompactSizeUInt.one,
                                   Vector(block.blockHeader))
                  for {
                    isIBD <- isChainIBD
                    newMsgHandler <- {
                      // if in IBD, do not process this header, just execute callbacks
                      if (!isIBD) {
                        handleDataPayload(payload = headersMessage,
                                          peerData = peerData)
                      } else {
                        appConfig.callBacks
                          .executeOnBlockHeadersReceivedCallbacks(
                            Vector(block.blockHeader))
                          .map(_ => this)
                      }
                    }
                  } yield {
                    newMsgHandler
                  }
                } else Future.successful(this)
              }
          }

          for {
            handler <- newMsgHandlerF
            _ <-
              appConfig.callBacks
                .executeOnBlockReceivedCallbacks(block)
          } yield {
            handler
          }
        case TransactionMessage(tx) =>
          logger.trace(
            s"Received txmsg=${tx.txIdBE}, processing given callbacks")
          appConfig.callBacks
            .executeOnTxReceivedCallbacks(tx)
            .map(_ => this)
        case MerkleBlockMessage(_) =>
          logger.warn(s"Merkleblock is not supported")
          Future.successful(this)
        case invMsg: InventoryMessage =>
          handleInventoryMsg(invMsg = invMsg, peer = peer)
      }
    }

    wrappedFuture.flatten
  }

  /** syncs filter headers in case the header chain is still ahead post filter sync */
  private def syncIfHeadersAhead(
      syncPeer: Peer,
      peers: Set[Peer]): Future[NodeState] = {
    for {
      headerHeight <- chainApi.getBestHashBlockHeight()
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      filterCount <- chainApi.getFilterCount()
      newState <- {
        require(headerHeight >= Math.max(filterHeaderCount, filterCount),
                "Header chain cannot be behind filter or filter header chain")
        require(
          filterHeaderCount >= filterCount,
          s"Filter header height $filterHeaderCount must be atleast filter height $filterCount")
        if (headerHeight > filterHeaderCount) {
          logger.info(
            s"Starting to fetch filter headers in data message handler")
          for {
            syncingFilterHeadersState <- PeerManager
              .sendFirstGetCompactFilterHeadersCommand(peerMessageSenderApi =
                                                         peerMessageSenderApi,
                                                       chainApi = chainApi,
                                                       peer = syncPeer,
                                                       peers = peers)
          } yield {
            syncingFilterHeadersState
          }

        } else {
          require(
            headerHeight == filterHeaderCount,
            s"headerHeight=$headerHeight filterHeaderCount=$filterHeaderCount")
          require(headerHeight == filterCount,
                  s"headerHeight=$headerHeight filterCount=$filterCount")
          logger.info(s"We are synced")
          //check to see if we had blocks mined while IBD
          //was ongoing, see: https://github.com/bitcoin-s/bitcoin-s/issues/5036
          for {
            bestBlockHash <- chainApi.getBestBlockHash()
            isIBD <- chainApi.isIBD()
            newState <- {
              if (isIBD) {
                peerMessageSenderApi
                  .gossipGetHeadersMessage(Vector(bestBlockHash))
                  .map { _ =>
                    //set to done syncing since we are technically done with IBD
                    //we just need to sync blocks that occurred while we were doing IBD
                    DoneSyncing(peers)
                  }
              } else {
                Future.successful(DoneSyncing(peers))
              }
            }
          } yield newState
        }
      }
    } yield newState
  }

  /** Recover the data message handler if we received an invalid block header from a peer */
  private def recoverInvalidHeader(
      peerData: PeerData): Future[DataMessageHandler] = {
    val result = state match {
      case state @ (HeaderSync(_, _) | DoneSyncing(_)) =>
        val peer = peerData.peer
        peerData.updateInvalidMessageCount()
        if (peerData.exceededMaxInvalidMessages) {
          logger.warn(
            s"$peer exceeded max limit of invalid messages. Disconnecting.")

          Future.successful(copy(state = MisbehavingPeer(peer, state.peers)))
        } else {
          logger.info(
            s"Re-querying headers from $peer. invalidMessages=${peerData.getInvalidMessageCount} peers.size=${state.peers.size}")
          for {
            blockchains <- BlockHeaderDAO().getBlockchains()
            cachedHeaders = blockchains
              .flatMap(_.headers)
              .map(_.hashBE)
            _ <- peerMessageSenderApi.sendGetHeadersMessage(cachedHeaders,
                                                            Some(peer))
          } yield this.copy(state = HeaderSync(peer, state.peers))
        }

      case headerState @ ValidatingHeaders(peer, _, failedCheck, _, _) =>
        //if a peer sends invalid data then mark it as failed, dont disconnect
        logger.warn(
          s"Got invalid headers from peer=$peer while validating. Marking as failed.")
        val newHeaderState =
          headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = copy(state = newHeaderState)

        if (newHeaderState.validated) {
          logger.info(
            s"Done validating headers, inSyncWith=${newHeaderState.inSyncWith}, failedCheck=${newHeaderState.failedCheck}")
          PeerManager.fetchCompactFilterHeaders(newDmh, peerMessageSenderApi)
        } else {
          Future.successful(newDmh)
        }
      case _: FilterHeaderSync | _: FilterSync =>
        Future.successful(this)
      case m @ (_: MisbehavingPeer | _: RemovePeers) =>
        val exn = new RuntimeException(
          s"Cannot recover invalid headers, got=$m")
        Future.failed(exn)
    }

    result
  }

  private def sendNextGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      syncPeer: Peer,
      prevStopHash: DoubleSha256DigestBE): Future[Boolean] =
    PeerManager.sendNextGetCompactFilterHeadersCommand(
      peerMessageSenderApi = peerMessageSenderApi,
      chainApi = chainApi,
      peer = syncPeer,
      filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
      prevStopHash = prevStopHash
    )

  private def sendNextGetCompactFilterCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      syncPeer: Peer,
      startHeight: Int,
      peers: Set[Peer]): Future[Option[NodeState.FilterSync]] = {

    PeerManager
      .sendNextGetCompactFilterCommand(
        peerMessageSenderApi = peerMessageSenderApi,
        chainApi = chainApi,
        filterBatchSize = chainConfig.filterBatchSize,
        startHeight = startHeight,
        peer = syncPeer)
      .map { isSyncing =>
        if (isSyncing)
          Some(NodeState.FilterSync(syncPeer, peers))
        else None
      }
  }

  private def sendFirstGetCompactFilterCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      syncPeer: Peer,
      startHeightOpt: Option[Int],
      peers: Set[Peer]): Future[Option[NodeState.FilterSync]] = {
    val startHeightF = startHeightOpt match {
      case Some(startHeight) => Future.successful(startHeight)
      case None              => chainApi.getFilterCount()
    }

    for {
      startHeight <- startHeightF
      _ = logger.info(
        s"Beginning to sync filters from startHeight=$startHeight")
      res <- sendNextGetCompactFilterCommand(peerMessageSenderApi =
                                               peerMessageSenderApi,
                                             syncPeer = syncPeer,
                                             startHeight = startHeight,
                                             peers = peers)
    } yield res
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peer: Peer): Future[DataMessageHandler] = {
    logger.debug(s"Received inv=${invMsg}")
    val getData = GetDataMessage(invMsg.inventories.flatMap {
      case Inventory(TypeIdentifier.MsgBlock, hash) =>
        appConfig.nodeType match {
          case NodeType.NeutrinoNode | NodeType.FullNode =>
            if (syncing) {
              logger.info(s"Ignoring inv message=$invMsg while in state=$state")
              None
            } else Some(Inventory(TypeIdentifier.MsgWitnessBlock, hash))
          case NodeType.BitcoindBackend =>
            throw new RuntimeException("This is impossible")
        }
      case Inventory(TypeIdentifier.MsgTx, hash) =>
        Some(Inventory(TypeIdentifier.MsgWitnessTx, hash))
      case other: Inventory => Some(other)
    })
    peerMessageSenderApi.sendMsg(getData, Some(peer)).map(_ => this)
  }

  private def getCompactFilterStartHeight(
      walletCreationTimeOpt: Option[Instant]): Future[Option[Int]] = {
    walletCreationTimeOpt match {
      case Some(instant) =>
        val creationTimeHeightF = chainApi
          .epochSecondToBlockHeight(instant.toEpochMilli / 1000)
        val filterCountF = chainApi.getFilterCount()
        for {
          creationTimeHeight <- creationTimeHeightF
          filterCount <- filterCountF
        } yield {
          //filterHeightOpt contains the height of the last filter of the last batch
          //so if we want to start syncing filters from the correct height we need to
          //decrease the computed height
          val height = Math.max(0, creationTimeHeight - 1)
          //want to choose the maximum out of these too
          //if our internal chainstate filter count is > creationTimeHeight
          //we just want to start syncing from our last seen filter
          Some(Math.max(height, filterCount))
        }
      case None =>
        Future.successful(None)
    }
  }

  private def calcFilterHeaderFilterHeight(
      chainApi: ChainApi): Future[(Int, Int)] = {
    for {
      filterHeaderHeight <- chainApi.getFilterHeaderCount()
      filterHeight <- chainApi.getFilterCount()
    } yield (filterHeaderHeight, filterHeight)
  }

  /** Helper method to determine if compact filters are synced */
  private def isFiltersSynced(
      chainApi: ChainApi,
      filterBatch: Set[CompactFilterMessage]): Future[Boolean] = {
    for {
      (newFilterHeaderHeight, newFilterHeight) <- calcFilterHeaderFilterHeight(
        chainApi)
      isSynced <-
        if (newFilterHeight == 0 && walletCreationTimeOpt.isDefined) {
          //if we have zero filters in our database and are syncing filters after a wallet creation time
          //we need to calculate the offset of the first filter
          //and how many compact filter headers we have seen. filter_height = best_filter_header - first_filter_filter_header
          val bestBlockHashF = chainApi.getBestBlockHash()
          val filterHeadersF: Future[Vector[CompactFilterHeaderDb]] = {
            Future
              .traverse(filterBatch)(f =>
                chainApi.getFilterHeader(f.blockHash.flip))
              .map(_.flatten.toVector)
          }

          for {
            bestBlockHash <- bestBlockHashF
            filterHeaders <- filterHeadersF
          } yield {
            filterHeaders.exists(_.blockHashBE == bestBlockHash)
          }
        } else if (newFilterHeight == 0 && walletCreationTimeOpt.isEmpty) {
          //fully syncing all filters
          Future.successful(filterBatch.size == newFilterHeaderHeight + 1)
        } else {
          Future.successful(
            (newFilterHeight + filterBatch.size) == newFilterHeaderHeight)
        }
    } yield {
      isSynced
    }

  }

  /** Checks if the IBD flag needs to be set from true -> false */
  private def checkIBD(chainApi: ChainApi): Future[Unit] = {
    val isSyncingF = chainApi.isSyncing()
    val isIBDF = chainApi.isIBD()
    for {
      isSyncing <- isSyncingF
      isIBD <- isIBDF
      _ <- {
        if (isIBD) {
          chainApi.setIBD(isSyncing)
        } else {
          Future.unit
        }
      }
    } yield ()
  }

  private def sortBlockFiltersByBlockHeight(
      filterBatch: Set[CompactFilterMessage]): Future[
    Vector[(DoubleSha256Digest, CompactFilterMessage, GolombFilter)]] = {
    val blockFiltersF: Future[
      Set[(Int, DoubleSha256Digest, CompactFilterMessage, GolombFilter)]] = {
      Future.traverse(filterBatch) { filter =>
        val blockHeightOptF =
          chainApi.getBlockHeight(filter.blockHash.flip)
        val filtersWithBlockHeightF = for {
          blockHeightOpt <- blockHeightOptF
        } yield {
          require(
            blockHeightOpt.isDefined,
            s"Could not find block height for blockHash=${filter.blockHash.flip}")
          (blockHeightOpt.get,
           filter.blockHash,
           filter,
           BlockFilter.fromBytes(filter.filterBytes, filter.blockHash))
        }

        filtersWithBlockHeightF
      }
    }
    val sortedBlockFiltersF = {
      blockFiltersF
        .map(_.toVector.sortBy(_._1))
        .map(set => set.map(tuple => (tuple._2, tuple._3, tuple._4)))
    }

    sortedBlockFiltersF
  }

  private def getHeaders(
      newDmh: DataMessageHandler,
      headers: Vector[BlockHeader],
      peer: Peer): Future[DataMessageHandler] = {
    logger.debug(
      s"getHeaders() newDmh.state=${newDmh.state} peer=$peer peers=$peer")
    val state = newDmh.state
    val count = headers.length
    val getHeadersF: Future[DataMessageHandler] = {
      val newApi = newDmh.chainApi
      if (headers.nonEmpty) {

        val lastHeader = headers.last
        val lastHash = lastHeader.hash
        newApi.getBlockCount().map { count =>
          logger.trace(
            s"Processed headers, most recent has height=$count and hash=$lastHash.")
        }

        if (count == HeadersMessage.MaxHeadersCount) {

          state match {
            case HeaderSync(_, _) =>
              logger.info(
                s"Received maximum amount of headers in one header message. This means we are not synced, requesting more")
              //ask for headers more from the same peer
              peerMessageSenderApi
                .sendGetHeadersMessage(lastHash.flip, Some(peer))
                .map(_ => newDmh)

            case ValidatingHeaders(_, inSyncWith, _, _, peers) =>
              //ask for more headers now
              val askF = peerMessageSenderApi
                .sendGetHeadersMessage(lastHash.flip, Some(peer))
                .map(_ => syncing)

              for {
                _ <- askF
              } yield {
                newDmh.copy(state =
                  RemovePeers(inSyncWith.toVector, peers, true))
              }
            case x @ (_: FilterHeaderSync | _: FilterSync | DoneSyncing(_) |
                _: MisbehavingPeer | _: RemovePeers) =>
              val exn = new RuntimeException(
                s"Cannot be in state=$x while retrieving block headers")
              Future.failed(exn)
          }

        } else {
          logger.debug(
            List(s"Received headers=${count.toInt} in one message,",
                 "which is less than max. This means we are synced,",
                 s"not requesting more. state=$state")
              .mkString(" "))
          // If we are in neutrino mode, we might need to start fetching filters and their headers
          // if we are syncing we should do this, however, sometimes syncing isn't a good enough check,
          // so we also check if our cached filter heights have been set as well, if they haven't then
          // we probably need to sync filters
          state match {
            case HeaderSync(syncPeer, peers) =>
              // headers are synced now with the current sync peer, now move to validating it for all peers
              require(syncPeer == peer, s"syncPeer=$syncPeer peer=$peer")

              if (peers.size > 1) {
                val newState =
                  ValidatingHeaders(
                    syncPeer = peer,
                    inSyncWith = Set(peer),
                    verifyingWith = peers.toSet,
                    failedCheck = Set.empty[Peer],
                    peers = peers
                  )

                logger.info(
                  s"Starting to validate headers now. Verifying with ${newState.verifyingWith}")

                val getHeadersAllF = {
                  val msg = GetHeadersMessage(lastHash)
                  peerMessageSenderApi.gossipMessage(msg,
                                                     excludedPeerOpt =
                                                       Some(peer))
                }

                getHeadersAllF
                  .map(_ => newDmh.copy(state = newState))
              } else {
                //if just one peer then can proceed ahead directly
                PeerManager.fetchCompactFilterHeaders(newDmh,
                                                      peerMessageSenderApi)
              }

            case headerState @ ValidatingHeaders(_, inSyncWith, _, _, _) =>
              //add the current peer to it
              val newHeaderState =
                headerState.copy(syncPeer = peer,
                                 inSyncWith = inSyncWith + peer)
              val newDmh2 = newDmh.copy(state = newHeaderState)

              if (newHeaderState.validated) {
                // If we are in neutrino mode, we might need to start fetching filters and their headers
                // if we are syncing we should do this, however, sometimes syncing isn't a good enough check,
                // so we also check if our cached filter heights have been set as well, if they haven't then
                // we probably need to sync filters

                PeerManager.fetchCompactFilterHeaders(currentDmh = newDmh2,
                                                      peerMessageSenderApi)
              } else {
                //do nothing, we are still waiting for some peers to send headers or timeout
                Future.successful(newDmh2)
              }
            case x @ (_: FilterHeaderSync | _: FilterSync | DoneSyncing(_) |
                _: MisbehavingPeer | _: RemovePeers) =>
              val exn = new RuntimeException(
                s"Cannot be in state=$x while we are about to begin syncing compact filter headers")
              Future.failed(exn)
          }
        }
      } else {
        //what if we are synced exactly by the 2000th header
        state match {
          case headerState @ ValidatingHeaders(_, inSyncWith, _, _, _) =>
            val newHeaderState =
              headerState.copy(inSyncWith = inSyncWith + peer, syncPeer = peer)
            val newDmh2 = newDmh.copy(state = newHeaderState)
            if (newHeaderState.validated) {
              PeerManager.fetchCompactFilterHeaders(newDmh2,
                                                    peerMessageSenderApi)
            } else {
              //do nothing, we are still waiting for some peers to send headers
              Future.successful(newDmh2)
            }
          case _: HeaderSync =>
            Future.successful(newDmh)
          case DoneSyncing(_) =>
            Future.successful(newDmh)
          case x @ (_: FilterHeaderSync | _: FilterSync | _: MisbehavingPeer |
              _: RemovePeers) =>
            val exn = new RuntimeException(
              s"Invalid state to complete block header sync in, got=$x")
            Future.failed(exn)
        }
      }
    }
    getHeadersF
  }

}
