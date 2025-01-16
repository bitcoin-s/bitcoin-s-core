package org.bitcoins.node.callback

import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{
  Keep,
  Sink,
  Source,
  SourceQueueWithComplete
}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.CallbackHandler
import org.bitcoins.core.api.callback.OnBlockReceived
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.*

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}

/** Creates a wrapper around the give node callbacks with a stream */
case class NodeCallbackStreamManager(
    callbacks: NodeCallbacks,
    overflowStrategy: OverflowStrategy = OverflowStrategy.backpressure,
    maxBufferSize: Int = 16
)(implicit system: ActorSystem)
    extends NodeCallbacks
    with StartStopAsync[Unit]
    with BitcoinSLogger {
  import system.dispatcher

  private val filterQueueSource: Source[
    Vector[
      (DoubleSha256DigestBE, GolombFilter)
    ],
    SourceQueueWithComplete[Vector[(DoubleSha256DigestBE, GolombFilter)]]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val filterSink
      : Sink[Vector[(DoubleSha256DigestBE, GolombFilter)], Future[Done]] = {
    Sink.foreachAsync(1) { case vec =>
      callbacks.executeOnCompactFiltersReceivedCallbacks(vec)
    }
  }

  private val (filterQueue, filterSinkCompleteF) =
    matSourceAndQueue(filterQueueSource, filterSink)

  private val txQueueSource
      : Source[Transaction, SourceQueueWithComplete[Transaction]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val txSink: Sink[Transaction, Future[Done]] = {
    Sink.foreachAsync(1) { case tx =>
      callbacks.executeOnTxReceivedCallbacks(tx)
    }
  }

  private val (txQueue, txSinkCompleteF) =
    matSourceAndQueue(txQueueSource, txSink)

  private val headerQueueSource
      : Source[Vector[
                 BlockHeader
               ],
               SourceQueueWithComplete[Vector[BlockHeader]]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val headerSink: Sink[Vector[BlockHeader], Future[Done]] = {
    Sink.foreachAsync(1) { case headers =>
      callbacks.executeOnBlockHeadersReceivedCallbacks(headers)
    }
  }

  private val (headerQueue, headerSinkCompleteF) =
    matSourceAndQueue(headerQueueSource, headerSink)

  private val blockQueueSource
      : Source[Block, SourceQueueWithComplete[Block]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val blockSink: Sink[Block, Future[Done]] = {
    Sink.foreachAsync(1) { case block =>
      callbacks.executeOnBlockReceivedCallbacks(block)
    }
  }

  private val (blockQueue, blockSinkCompleteF) =
    matSourceAndQueue(blockQueueSource, blockSink)

  private val merkleBlockQueueSource: Source[(MerkleBlock, Vector[Transaction]),
                                             SourceQueueWithComplete[
                                               (MerkleBlock,
                                                Vector[Transaction])
                                             ]] = {
    Source.queue(maxBufferSize, overflowStrategy)
  }

  private val merkleBlockSink
      : Sink[(MerkleBlock, Vector[Transaction]), Future[Done]] = {
    Sink.foreachAsync(1) { case tuple =>
      callbacks.executeOnMerkleBlockReceivedCallbacks(
        merkleBlock = tuple._1,
        txs = tuple._2
      )
    }
  }

  private val (merkleBlockQueue, merkleBlockCompleteF) =
    matSourceAndQueue(merkleBlockQueueSource, merkleBlockSink)

  override def start(): Future[Unit] = Future.unit

  private val isStopped: AtomicBoolean = new AtomicBoolean(false)

  /** Completes all streams and waits until they are fully drained */
  override def stop(): Future[Unit] = {
    val start = System.currentTimeMillis()

    // can't complete a stream twice
    if (!isStopped.get()) {
      // complete all queues
      filterQueue.complete()
      txQueue.complete()
      headerQueue.complete()
      merkleBlockQueue.complete()
      blockQueue.complete()
      isStopped.set(true)
    }

    for {
      _ <- filterSinkCompleteF
      _ <- txSinkCompleteF
      _ <- headerSinkCompleteF
      _ <- merkleBlockCompleteF
      _ <- blockSinkCompleteF
    } yield {
      logger.info(
        s"Done draining akka streams for NodeCallbackStreamManager, it took=${System
            .currentTimeMillis() - start}ms"
      )
      ()
    }
  }

  private def matSourceAndQueue[T](
      source: Source[T, SourceQueueWithComplete[T]],
      sink: Sink[T, Future[Done]]
  ): (SourceQueueWithComplete[T], Future[Done]) = {
    source
      .toMat(sink)(Keep.both)
      .run()
  }

  override def onCompactFiltersReceived
      : CallbackHandler[Vector[
                          (DoubleSha256DigestBE, GolombFilter)
                        ],
                        OnCompactFiltersReceived] = {
    callbacks.onCompactFiltersReceived
  }

  override def onTxReceived: CallbackHandler[Transaction, OnTxReceived] = {
    callbacks.onTxReceived
  }

  override def onBlockReceived: CallbackHandler[Block, OnBlockReceived] =
    callbacks.onBlockReceived

  override def onMerkleBlockReceived: CallbackHandler[
    (MerkleBlock, Vector[Transaction]),
    OnMerkleBlockReceived
  ] = callbacks.onMerkleBlockReceived

  override def onBlockHeadersReceived
      : CallbackHandler[Vector[BlockHeader], OnBlockHeadersReceived] =
    callbacks.onBlockHeadersReceived

  override def executeOnTxReceivedCallbacks(
      tx: Transaction
  )(implicit ec: ExecutionContext): Future[Unit] = {
    txQueue
      .offer(tx)
      .map(_ => ())
  }

  override def executeOnBlockReceivedCallbacks(
      block: Block
  )(implicit ec: ExecutionContext): Future[Unit] = {
    blockQueue
      .offer(block)
      .map(_ => ())
  }

  override def executeOnCompactFiltersReceivedCallbacks(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    filterQueue
      .offer(blockFilters)
      .map(_ => ())
  }

  override def executeOnMerkleBlockReceivedCallbacks(
      merkleBlock: MerkleBlock,
      txs: Vector[Transaction]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    merkleBlockQueue
      .offer((merkleBlock, txs))
      .map(_ => ())
  }

  override def executeOnBlockHeadersReceivedCallbacks(
      headers: Vector[BlockHeader]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    headerQueue
      .offer(headers)
      .map(_ => ())
  }

  /** Consider calling [[stop()]] before creating a new instance of callbacks
    * this is because the previous stream will keep running and a new stream
    * will be created
    */
  override def +(other: NodeCallbacks): NodeCallbacks = {
    val newCallbacks = other.+(this)
    NodeCallbackStreamManager(newCallbacks)
  }
}
