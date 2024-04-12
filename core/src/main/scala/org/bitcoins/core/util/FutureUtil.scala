package org.bitcoins.core.util

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

object FutureUtil {

  val unit: Future[Unit] = Future.successful(())

  /** Executes a series of futures sequentially
    *
    * @param items The elements we want to transform into futures
    * @param fun A function that transforms each element into a future
    * @return The processed elements
    */
  def sequentially[T, U](items: Iterable[T])(fun: T => Future[U])(implicit
      ec: ExecutionContext): Future[Vector[U]] = {
    val init = Future.successful(Vector.empty[U])
    items.foldLeft(init) { (f, item) =>
      f.flatMap { x =>
        fun(item).map(_ +: x)
      }
    } map (_.reverse)
  }

  /** Executes a series of futures sequentially. It's similar to [[FutureUtil.sequentially()]],
    * but it accepts a collection of futures and executes them one by one.
    * @param items The collection of futures
    * @return The processed elements
    */
  def collect[T](items: Iterable[Future[T]])(implicit
      ec: ExecutionContext): Future[Vector[T]] = {
    FutureUtil.sequentially(items)(x => x)
  }

  def none[T]: Future[Option[T]] = Future.successful(Option.empty[T])

  def emptyVec[T]: Future[Vector[T]] = Future.successful(Vector.empty[T])

  /** Folds over the given elements sequentially in a non-blocking async way
    * @param init the initialized value for the accumulator
    * @param items the items we are folding over
    * @param fun the function we are applying to every element that returns a future
    * @return
    */
  def foldLeftAsync[T, U](init: T, items: Seq[U])(fun: (T, U) => Future[T])(
      implicit ec: ExecutionContext): Future[T] = {
    items.foldLeft(Future.successful(init)) { case (accumF, elem) =>
      accumF.flatMap { accum =>
        fun(accum, elem)
      }
    }
  }

  /** Takes elements, groups them into batches of 'batchSize' and then calls f on them.
    * The next batch does not start executing until the first batch is finished. This does
    * not aggregate result over batches, rather just returns the result of the last batch
    */
  def batchExecute[T, U](
      elements: Vector[T],
      f: Vector[T] => Future[U],
      init: U,
      batchSize: Int)(implicit ec: ExecutionContext): Future[U] = {
    val initF = Future.successful(init)
    val batches = elements.grouped(batchSize)
    for {
      batchExecution <- {
        batches.foldLeft(initF) { case (uF, batch) =>
          for {
            _ <- uF
            executed <- f(batch)
          } yield executed
        }
      }
    } yield batchExecution
  }

  /** Batches the elements by batchSize, executes f, and then aggregates all of the results
    * into a vector and returns it. This is is the synchronous version of batchAndParallelExecute
    */
  def batchAndSyncExecute[T, U](
      elements: Vector[T],
      f: Vector[T] => Future[Vector[U]],
      batchSize: Int)(implicit ec: ExecutionContext): Future[Vector[U]] = {
    val initF = Future.successful(Vector.empty)
    val batches = elements.grouped(batchSize)
    for {
      batchExecution <- {
        batches.foldLeft[Future[Vector[U]]](initF) {
          case (accumF: Future[Vector[U]], batch: Vector[T]) =>
            for {
              accum <- accumF
              executed <- f(batch)
            } yield {
              accum ++ executed
            }
        }
      }
    } yield batchExecution
  }

  def makeAsync[T](func: () => T)(implicit ec: ExecutionContext): Future[T] = {
    val resultP = Promise[T]()

    ec.execute { () =>
      val resultT = Try(func())
      resultP.complete(resultT)
    }

    resultP.future
  }

  /** Batches the [[elements]] by [[batchSize]] and then calls [[f]] on them in parallel
    * This is the parallel version of [[batchAndSyncExecute()]]
    */
  def batchAndParallelExecute[T, U](
      elements: Vector[T],
      f: Vector[T] => Future[U],
      batchSize: Int)(implicit ec: ExecutionContext): Future[Vector[U]] = {
    require(
      batchSize > 0,
      s"Cannot have batch size less than or equal to zero, got=$batchSize")
    if (elements.isEmpty) {
      Future.successful(Vector.empty)
    } else {
      val batches = elements.grouped(batchSize).toVector
      val executeF: Future[Vector[U]] = Future.traverse(batches)(f(_))
      executeF
    }
  }

  /** Same as [[batchAndParallelExecute()]], but computes the batchSize based on the
    * number of available processors on your machine
    */
  def batchAndParallelExecute[T, U](
      elements: Vector[T],
      f: Vector[T] => Future[U])(implicit
      ec: ExecutionContext): Future[Vector[U]] = {
    //divide and conquer
    val batchSize =
      Math.max(elements.length / Runtime.getRuntime.availableProcessors(), 1)

    batchAndParallelExecute(elements, f, batchSize)
  }

  def getParallelism: Int = {
    val processors = Runtime.getRuntime.availableProcessors()
    //max open requests is 32 in akka, so 1/8 of possible requests
    //can be used to open http requests in akka, else just limit it be number of processors
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/4252
    Math.min(4, processors)
  }
}
