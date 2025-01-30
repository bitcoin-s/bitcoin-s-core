package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.EventDescriptorTLV
import org.bitcoins.crypto._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class OracleAnnouncementDataDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUDAutoInc[OracleAnnouncementDataDb] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[OracleAnnouncementsTable] =
    TableQuery[OracleAnnouncementsTable]

  def findByPublicKey(
      publicKey: SchnorrPublicKey
  ): Future[Vector[OracleAnnouncementDataDb]] = {
    val query = table.filter(_.publicKey === publicKey)

    safeDatabase.runVec(query.result)
  }

  def findByAnnouncementSignatures(
      signatures: Vector[SchnorrDigitalSignature]
  ): Future[Vector[OracleAnnouncementDataDb]] = {
    val query = table.filter(_.announcementSignature.inSet(signatures))

    safeDatabase
      .runVec(query.result)
  }

  def findByIds(ids: Vector[Long]): Future[Vector[OracleAnnouncementDataDb]] = {
    safeDatabase.run(findByIdsAction(ids))
  }

  def findByIdsAction(
      ids: Vector[Long]
  ): DBIOAction[Vector[OracleAnnouncementDataDb], NoStream, Effect.Read] = {
    table.filter(_.id.inSet(ids)).result.map(_.toVector)
  }

  def findById(id: Long): Future[Option[OracleAnnouncementDataDb]] = {
    findByIds(Vector(id)).map(_.headOption)
  }

  class OracleAnnouncementsTable(tag: Tag)
      extends TableAutoInc(
        tag,
        schemaName,
        "oracle_announcement_data"
      ) {

    def announcementSignature: Rep[SchnorrDigitalSignature] = column(
      "announcement_signature"
    )

    def publicKey: Rep[SchnorrPublicKey] = column("pub_key")

    def signingPublicKey: Rep[SchnorrPublicKey] = column("signing_pub_key")

    def eventId: Rep[String] = column("event_id")

    def eventDescriptor: Rep[EventDescriptorTLV] = column("event_descriptor")

    def eventMaturity: Rep[UInt32] = column("event_maturity")

    override def * : ProvenShape[OracleAnnouncementDataDb] =
      (
        id.?,
        announcementSignature,
        publicKey,
        signingPublicKey,
        eventId,
        eventDescriptor,
        eventMaturity
      )
        .<>(OracleAnnouncementDataDb.apply, OracleAnnouncementDataDb.unapply)
  }
}
