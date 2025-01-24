package org.bitcoins.db

import org.bitcoins.core.api.db.DbRowAutoInc

import scala.concurrent.{ExecutionContext, Future}

abstract class CRUDAutoInc[T <: DbRowAutoInc[T]](implicit
    ec: ExecutionContext,
    override val appConfig: DbAppConfig
) extends CRUD[T, Long]()(ec, appConfig)
    with TableAutoIncComponent[T] {
  import profile.api._

  /** The table inside our database we are inserting into */
  override val table: profile.api.TableQuery[? <: TableAutoInc]

  override def createAllAction(
      ts: Vector[T]
  ): profile.api.DBIOAction[Vector[T], profile.api.NoStream, Effect.Write] = {
    val idQuery = table.map(_.id)
    val idAutoInc = table.returning(idQuery)
    val query = {
      idAutoInc.into((t, id) => t.copyWithId(id = id))
    }
    val actions = query.++=(ts)
    actions.map(_.toVector)
  }

  override def createAll(ts: Vector[T]): Future[Vector[T]] = {
    val actions = createAllAction(ts)
    safeDatabase.runVec(actions)
  }

  override protected def findByPrimaryKey(
      id: Long
  ): Query[TableAutoInc, T, Seq] =
    table
      .filter(_.id === id)
      .asInstanceOf[Query[TableAutoInc, T, Seq]]

  override def findByPrimaryKeys(
      ids: Vector[Long]
  ): Query[TableAutoInc, T, Seq] = {
    table
      .filter { t =>
        t.id.inSet(ids)
      }
      .asInstanceOf[Query[TableAutoInc, T, Seq]]
  }

  override def findAll(ts: Vector[T]): Query[Table[T], T, Seq] = {
    val ids = ts.filter(_.id.isDefined).map(_.id.get)
    findByPrimaryKeys(ids)
  }
}

/** Defines a table that has an auto incremented fields that is named id. This
  * is useful for things we want to store that don't have an inherent id such as
  * a hash.
  * @param tag
  * @param tableName
  * @tparam T
  */
trait TableAutoIncComponent[T <: DbRowAutoInc[T]] { self: CRUDAutoInc[T] =>
  import profile.api._

  abstract class TableAutoInc(
      tag: profile.api.Tag,
      schemaName: Option[String],
      tableName: String
  ) extends profile.api.Table[T](tag, schemaName, tableName) {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
  }
}
