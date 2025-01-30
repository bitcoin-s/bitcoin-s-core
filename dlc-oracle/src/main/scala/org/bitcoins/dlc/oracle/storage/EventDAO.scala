package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.db.{EventDb, RValueDb}
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv.{
  EventDescriptorTLV,
  OracleEventTLV,
  OracleEventV0TLV
}
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, DbCommonsColumnMappers, SlickUtil}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class EventDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCOracleAppConfig
) extends CRUD[EventDb, SchnorrNonce]
    with SlickUtil[EventDb, SchnorrNonce] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[EventTable] = TableQuery[EventTable]

  private lazy val rValueTable: TableQuery[RValueDAO#RValueTable] =
    RValueDAO().table

  override def createAll(ts: Vector[EventDb]): Future[Vector[EventDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[SchnorrNonce]
  ): Query[EventTable, EventDb, Seq] =
    table.filter(_.nonce.inSet(ids))

  override protected def findAll(
      ts: Vector[EventDb]
  ): Query[EventTable, EventDb, Seq] =
    findByPrimaryKeys(ts.map(_.nonce))

  def getPendingEvents: Future[Vector[EventDb]] = {
    val query = table.filter(_.attestationOpt.isEmpty)

    safeDatabase.runVec(query.result)
  }

  def getCompletedEvents: Future[Vector[EventDb]] = {
    val query = table.filter(_.attestationOpt.isDefined)

    safeDatabase.runVec(query.result)
  }

  def findByEventName(name: String): Future[Vector[EventDb]] = {
    val query = table.filter(_.eventName === name)

    safeDatabase.runVec(query.result)
  }

  def findByEventDescriptor(
      descriptorTLV: EventDescriptorTLV
  ): Future[Vector[EventDb]] = {
    val query = table.filter(_.eventDescriptorTLV === descriptorTLV)

    safeDatabase.runVec(query.result)
  }

  def findByOracleEventTLV(
      oracleEvent: OracleEventTLV
  ): Future[Vector[EventDb]] = {
    val query = oracleEvent match {
      case v0: OracleEventV0TLV =>
        table.filter(_.nonce.inSet(v0.nonces))
    }

    safeDatabase.runVec(query.result)
  }

  def findDifferentPublicKey(key: SchnorrPublicKey): Future[Vector[EventDb]] = {
    val query = table.filterNot(_.pubkey === key)

    safeDatabase.runVec(query.result)
  }

  class EventTable(tag: Tag) extends Table[EventDb](tag, schemaName, "events") {

    def nonce: Rep[SchnorrNonce] = column("nonce", O.PrimaryKey)

    def pubkey: Rep[SchnorrPublicKey] = column("pubkey")

    def nonceIndex: Rep[Int] = column("nonce_index")

    def eventName: Rep[String] = column("event_name")

    def numOutcomes: Rep[Long] = column("num_outcomes")

    def signingVersion: Rep[SigningVersion] = column("signing_version")

    def maturationTime: Rep[Instant] = column("maturation_time")

    def attestationOpt: Rep[Option[FieldElement]] = column("attestation")

    def outcomeOpt: Rep[Option[String]] = column("outcome")

    def announcementSignature: Rep[SchnorrDigitalSignature] =
      column("announcement_signature")

    def eventDescriptorTLV: Rep[EventDescriptorTLV] =
      column("event_descriptor_tlv")

    def * : ProvenShape[EventDb] =
      (
        nonce,
        pubkey,
        nonceIndex,
        eventName,
        numOutcomes,
        signingVersion,
        maturationTime,
        attestationOpt,
        outcomeOpt,
        announcementSignature,
        eventDescriptorTLV
      ).<>(EventDb.apply, EventDb.unapply)

    def fk: ForeignKeyQuery[?, RValueDb] = {
      foreignKey(
        "fk_nonce",
        sourceColumns = nonce,
        targetTableQuery = rValueTable
      )(_.nonce)
    }

    def fkLabel: ForeignKeyQuery[?, RValueDb] = {
      foreignKey(
        "fk_label",
        sourceColumns = eventName,
        targetTableQuery = rValueTable
      )(_.eventName)
    }
  }
}
