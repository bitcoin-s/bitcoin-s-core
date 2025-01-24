package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class IncomingDLCOfferDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[IncomingDLCOfferDb, Sha256Digest]
    with SlickUtil[IncomingDLCOfferDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[IncomingDLCOfferTable] =
    TableQuery[IncomingDLCOfferTable]

  override def createAll(
      ts: Vector[IncomingDLCOfferDb]
  ): Future[Vector[IncomingDLCOfferDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findAllAction()
      : DBIOAction[Vector[IncomingDLCOfferDb], NoStream, Effect.Read] =
    table.sortBy(_.receivedAt.desc).result.map(_.toVector)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256Digest]
  ): Query[Table[IncomingDLCOfferDb], IncomingDLCOfferDb, Seq] =
    table.filter(_.hash.inSet(ids))

  override protected def findAll(
      ts: Vector[IncomingDLCOfferDb]
  ): Query[Table[IncomingDLCOfferDb], IncomingDLCOfferDb, Seq] =
    findByPrimaryKeys(ts.map(_.hash))

  def delete(pk: Sha256Digest): Future[Int] = {
    val query = table.filter(_.hash === pk)
    safeDatabase.run(query.delete)
  }

  def find(pk: Sha256Digest): Future[Option[IncomingDLCOfferDb]] = {
    val query = table.filter(_.hash === pk)
    safeDatabase.run(query.result).map(_.headOption)
  }

  class IncomingDLCOfferTable(tag: Tag)
      extends Table[IncomingDLCOfferDb](tag, schemaName, "incoming_offers") {

    def hash: Rep[Sha256Digest] = column("hash", O.PrimaryKey)

    def receivedAt: Rep[Instant] = column("received_at")

    def peer: Rep[Option[String]] = column("peer")

    def message: Rep[Option[String]] = column("message")

    def offerTLV: Rep[DLCOfferTLV] = column("offer_tlv")

    override def * =
      (
        hash,
        receivedAt,
        peer,
        message,
        offerTLV
      ) <> (IncomingDLCOfferDb.apply, IncomingDLCOfferDb.unapply)
  }
}
