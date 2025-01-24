package org.bitcoins.db

import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait SlickUtilAction[T, PrimaryKeyType] {
  this: CRUDAction[T, PrimaryKeyType] =>

  def createAllAction(
      ts: Vector[T]): DBIOAction[Vector[T], NoStream, Effect.Write] = {
    import profile.api._
    val fixedSqlAction = table ++= ts

    fixedSqlAction.map(_ => ts)
  }
}

trait SlickUtil[T, PrimaryKeyType] extends SlickUtilAction[T, PrimaryKeyType] {
  this: CRUD[T, PrimaryKeyType] =>

  import profile.api._

  /** Creates rows in a database that are not auto incremented */
  def createAllNoAutoInc(ts: Vector[T], database: SafeDatabase)(implicit
      ec: ExecutionContext): Future[Vector[T]] = {
    val actions = (table ++= ts).andThen(DBIO.successful(ts))
    val result = database.run(actions)
    result
  }
}
