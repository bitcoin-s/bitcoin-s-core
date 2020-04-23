package org.bitcoins.node

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.MarkedLogger
import org.bitcoins.node.networking.peer.DataMessageHandler._

import scala.concurrent.{ExecutionContext, Future}

/**
  * Callbacks for responding to events in the SPV node.
  * The approriate callback is executed whenver the node receives
  * a `getdata` message matching it.
  *
  */
case class NodeCallbacks(
    onCompactFilterReceived: Seq[OnCompactFiltersReceived] = Seq.empty,
    onTxReceived: Seq[OnTxReceived] = Seq.empty,
    onBlockReceived: Seq[OnBlockReceived] = Seq.empty,
    onMerkleBlockReceived: Seq[OnMerkleBlockReceived] = Seq.empty,
    onBlockHeadersReceived: Seq[OnBlockHeadersReceived] = Seq.empty
) {

  def +(other: NodeCallbacks): NodeCallbacks = copy(
    onCompactFilterReceived = onCompactFilterReceived ++ other.onCompactFilterReceived,
    onTxReceived = onTxReceived ++ other.onTxReceived,
    onBlockReceived = onBlockReceived ++ other.onBlockReceived,
    onMerkleBlockReceived = onMerkleBlockReceived ++ other.onMerkleBlockReceived,
    onBlockHeadersReceived = onBlockHeadersReceived ++ other.onBlockHeadersReceived
  )

  def executeOnTxReceivedCallbacks(logger: MarkedLogger, tx: Transaction)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onTxReceived
      .foldLeft(FutureUtil.unit)((acc, callback) =>
        acc.flatMap(_ =>
          callback(tx).recover {
            case err: Throwable =>
              logger.error("onTxReceived Callback failed with error: ", err)
          }))
  }

  def executeOnBlockReceivedCallbacks(logger: MarkedLogger, block: Block)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onBlockReceived
      .foldLeft(FutureUtil.unit)((acc, callback) =>
        acc.flatMap(_ =>
          callback(block).recover {
            case err: Throwable =>
              logger.error("onBlockReceived Callback failed with error: ", err)
          }))
  }

  def executeOnMerkleBlockReceivedCallbacks(
      logger: MarkedLogger,
      merkleBlock: MerkleBlock,
      txs: Vector[Transaction])(implicit ec: ExecutionContext): Future[Unit] = {
    onMerkleBlockReceived
      .foldLeft(FutureUtil.unit)((acc, callback) =>
        acc.flatMap(_ =>
          callback(merkleBlock, txs).recover {
            case err: Throwable =>
              logger.error("OnMerkleBlockReceived Callback failed with error: ",
                           err)
          }))
  }

  def executeOnCompactFiltersReceivedCallbacks(
      logger: MarkedLogger,
      blockHashes: Vector[DoubleSha256Digest],
      blockFilters: Vector[GolombFilter])(
      implicit ec: ExecutionContext): Future[Unit] = {
    require(
      blockHashes.size == blockFilters.size,
      s"every filter must have a block hash, ${blockHashes.size} != ${blockFilters.size}")
    onCompactFilterReceived
      .foldLeft(FutureUtil.unit)((acc, callback) =>
        acc.flatMap(_ =>
          callback(blockHashes, blockFilters).recover {
            case err: Throwable =>
              logger.error(
                "onCompactFiltersReceived Callback failed with error: ",
                err)
          }))
  }

  def executeOnBlockHeadersReceivedCallbacks(
      logger: MarkedLogger,
      headers: Vector[BlockHeader])(
      implicit ec: ExecutionContext): Future[Unit] = {
    onBlockHeadersReceived
      .foldLeft(FutureUtil.unit)((acc, callback) =>
        acc.flatMap(_ =>
          callback(headers).recover {
            case err: Throwable =>
              logger.error(
                "onBlockHeadersReceived Callback failed with error: ",
                err)
          }))
  }
}

object NodeCallbacks {

  /** Constructs a set of callbacks that only acts on TX received */
  def onTxReceived(f: OnTxReceived): NodeCallbacks =
    NodeCallbacks(onTxReceived = Seq(f))

  /** Constructs a set of callbacks that only acts on block received */
  def onBlockReceived(f: OnBlockReceived): NodeCallbacks =
    NodeCallbacks(onBlockReceived = Seq(f))

  /** Constructs a set of callbacks that only acts on merkle block received */
  def onMerkleBlockReceived(f: OnMerkleBlockReceived): NodeCallbacks =
    NodeCallbacks(onMerkleBlockReceived = Seq(f))

  /** Constructs a set of callbacks that only acts on compact filter received */
  def onCompactFilterReceived(f: OnCompactFiltersReceived): NodeCallbacks =
    NodeCallbacks(onCompactFilterReceived = Seq(f))

  /** Constructs a set of callbacks that only acts on block headers received */
  def onBlockHeadersReceived(f: OnBlockHeadersReceived): NodeCallbacks =
    NodeCallbacks(onBlockHeadersReceived = Seq(f))

  /** Empty callbacks that does nothing with the received data */
  val empty: NodeCallbacks =
    NodeCallbacks(
      onTxReceived = Seq.empty,
      onBlockReceived = Seq.empty,
      onMerkleBlockReceived = Seq.empty,
      onCompactFilterReceived = Seq.empty,
      onBlockHeadersReceived = Seq.empty
    )
}
