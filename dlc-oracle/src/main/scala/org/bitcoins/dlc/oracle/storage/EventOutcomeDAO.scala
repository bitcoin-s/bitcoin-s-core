package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.db.{EventDb, EventOutcomeDb}
import org.bitcoins.crypto.SchnorrNonce
import org.bitcoins.db.{CRUD, DbCommonsColumnMappers, SlickUtil}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import scodec.bits.ByteVector
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class EventOutcomeDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCOracleAppConfig
) extends CRUD[EventOutcomeDb, (SchnorrNonce, String)]
    with SlickUtil[EventOutcomeDb, (SchnorrNonce, String)] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[EventOutcomeTable] =
    TableQuery[EventOutcomeTable]

  private lazy val eventTable: TableQuery[EventDAO#EventTable] =
    EventDAO().table

  override def createAll(
      ts: Vector[EventOutcomeDb]
  ): Future[Vector[EventOutcomeDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[(SchnorrNonce, String)]
  ): Query[EventOutcomeTable, EventOutcomeDb, Seq] =
    table
      .filter(_.nonce.inSet(ids.map(_._1)))
      .filter(_.message.inSet(ids.map(_._2)))

  override protected def findAll(
      ts: Vector[EventOutcomeDb]
  ): Query[EventOutcomeTable, EventOutcomeDb, Seq] = {
    val ids = ts.map(t => (t.nonce, t.message))
    findByPrimaryKeys(ids)
  }

  def findByNonce(nonce: SchnorrNonce): Future[Vector[EventOutcomeDb]] = {
    findByNonces(Vector(nonce))
  }

  def findByNonces(
      nonces: Vector[SchnorrNonce]
  ): Future[Vector[EventOutcomeDb]] = {
    val action = table.filter(_.nonce.inSet(nonces)).result
    safeDatabase.runVec(action)
  }

  def find(
      nonce: SchnorrNonce,
      hash: ByteVector
  ): Future[Option[EventOutcomeDb]] = {
    val query =
      table.filter(item => item.nonce === nonce && item.hashedMessage === hash)

    safeDatabase.run(query.result).map(_.headOption)
  }

  class EventOutcomeTable(tag: Tag)
      extends Table[EventOutcomeDb](tag, schemaName, "event_outcomes") {

    def nonce: Rep[SchnorrNonce] = column("nonce")

    def message: Rep[String] = column("message")

    def hashedMessage: Rep[ByteVector] = column("hashed_message")

    def * : ProvenShape[EventOutcomeDb] =
      (nonce, message, hashedMessage).<>(
        EventOutcomeDb.apply,
        EventOutcomeDb.unapply
      )

    def fk: ForeignKeyQuery[?, EventDb] = {
      foreignKey(
        "fk_nonce",
        sourceColumns = nonce,
        targetTableQuery = eventTable
      )(_.nonce)
    }
  }
}
