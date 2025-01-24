package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class OracleNoncePrimaryKey(announcementId: Long, index: Long)

case class OracleNonceDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[OracleNonceDb, OracleNoncePrimaryKey]
    with SlickUtil[OracleNonceDb, OracleNoncePrimaryKey] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[OracleNoncesTable] =
    TableQuery[OracleNoncesTable]

  private lazy val announcementDataTable: slick.lifted.TableQuery[
    OracleAnnouncementDataDAO#OracleAnnouncementsTable
  ] = {
    OracleAnnouncementDataDAO().table
  }

  override def createAll(
      ts: Vector[OracleNonceDb]
  ): Future[Vector[OracleNonceDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[OracleNoncePrimaryKey]
  ): profile.api.Query[profile.api.Table[OracleNonceDb], OracleNonceDb, Seq] = {

    // is there a better way to do this?
    val starting = table.filterNot(_.announcementId === -1L)

    ids.foldLeft(starting) {
      case (accum, OracleNoncePrimaryKey(announcementId, index)) =>
        accum.flatMap(_ =>
          table.filter(t =>
            t.announcementId === announcementId && t.index === index))
    }
  }

  override protected def findByPrimaryKey(
      id: OracleNoncePrimaryKey
  ): profile.api.Query[profile.api.Table[OracleNonceDb], OracleNonceDb, Seq] = {
    table.filter(t =>
      t.announcementId === id.announcementId && t.index === id.index)
  }

  override def find(
      t: OracleNonceDb
  ): Query[Table[OracleNonceDb], OracleNonceDb, Seq] = {
    findByPrimaryKey(OracleNoncePrimaryKey(t.announcementId, t.index))
  }

  override protected def findAll(
      ts: Vector[OracleNonceDb]
  ): Query[Table[OracleNonceDb], OracleNonceDb, Seq] =
    findByPrimaryKeys(
      ts.map(t => OracleNoncePrimaryKey(t.announcementId, t.index))
    )

  def findByNonce(nonce: SchnorrNonce): Future[Option[OracleNonceDb]] = {
    findByNonces(Vector(nonce)).map(_.headOption)
  }

  def findByNoncesAction(
      nonces: Vector[SchnorrNonce]
  ): DBIOAction[Vector[OracleNonceDb], NoStream, Effect.Read] = {
    val query = table.filter(_.nonce.inSet(nonces))
    query.result.map(_.toVector)
  }

  def findByNonces(
      nonces: Vector[SchnorrNonce]
  ): Future[Vector[OracleNonceDb]] = {
    val action = findByNoncesAction(nonces)
    safeDatabase.runVec(action)
  }

  def findByAnnouncementId(id: Long): Future[Vector[OracleNonceDb]] = {
    findByAnnouncementIds(Vector(id))
  }

  def findByAnnouncementIds(
      ids: Vector[Long]
  ): Future[Vector[OracleNonceDb]] = {
    safeDatabase.run(findByAnnouncementIdsAction(ids))
  }

  def findByAnnouncementIdsAction(
      ids: Vector[Long]
  ): DBIOAction[Vector[OracleNonceDb], NoStream, Effect.Read] = {
    table.filter(_.announcementId.inSet(ids)).result.map(_.toVector)
  }

  class OracleNoncesTable(tag: Tag)
      extends Table[OracleNonceDb](tag, schemaName, "oracle_nonces") {

    def announcementId: Rep[Long] = column("announcement_id")

    def index: Rep[Long] = column("index")

    def announcementSignature: Rep[SchnorrDigitalSignature] = column(
      "announcement_signature"
    )

    def nonce: Rep[SchnorrNonce] = column("nonce", O.Unique)

    def signature: Rep[Option[SchnorrDigitalSignature]] = column("signature")

    def outcome: Rep[Option[String]] = column("outcome")

    override def * : ProvenShape[OracleNonceDb] =
      (announcementId, index, announcementSignature, nonce, signature, outcome)
        .<>(OracleNonceDb.apply, OracleNonceDb.unapply)

    def pk: PrimaryKey =
      primaryKey(
        name = "pk_oracle_nonces",
        sourceColumns = (announcementId, index)
      )

    def fk =
      foreignKey(
        "fk_announcement_id",
        sourceColumns = announcementId,
        targetTableQuery = announcementDataTable
      )(_.id)
  }
}
