package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.crypto._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class DLCAnnouncementPrimaryKey(dlcId: Sha256Digest, announcementId: Long)

case class DLCAnnouncementDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[DLCAnnouncementDb, DLCAnnouncementPrimaryKey]
    with SlickUtil[DLCAnnouncementDb, DLCAnnouncementPrimaryKey]
    with DLCIdDaoUtilNoPK[DLCAnnouncementDb] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCAnnouncementTable] =
    TableQuery[DLCAnnouncementTable]

  private lazy val announcementDataTable: slick.lifted.TableQuery[
    OracleAnnouncementDataDAO#OracleAnnouncementsTable
  ] = {
    OracleAnnouncementDataDAO().table
  }

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCAnnouncementDb]
  ): Future[Vector[DLCAnnouncementDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[DLCAnnouncementPrimaryKey]
  ): profile.api.Query[DLCAnnouncementTable, DLCAnnouncementDb, Seq] = {

    // is there a better way to do this?
    val starting = table.filterNot(_.dlcId === Sha256Digest.empty)

    ids.foldLeft(starting) {
      case (accum, DLCAnnouncementPrimaryKey(dlcId, announcementId)) =>
        accum.flatMap(_ =>
          table.filter(t =>
            t.dlcId === dlcId &&
              t.announcementId === announcementId))
    }
  }

  override def findByPrimaryKey(
      id: DLCAnnouncementPrimaryKey
  ): Query[DLCAnnouncementTable, DLCAnnouncementDb, Seq] = {
    table.filter(t =>
      t.dlcId === id.dlcId && t.announcementId === id.announcementId)
  }

  override def find(
      t: DLCAnnouncementDb
  ): profile.api.Query[Table[DLCAnnouncementDb], DLCAnnouncementDb, Seq] = {
    findByPrimaryKey(DLCAnnouncementPrimaryKey(t.dlcId, t.announcementId))
  }

  override protected def findAll(
      ts: Vector[DLCAnnouncementDb]
  ): Query[DLCAnnouncementTable, DLCAnnouncementDb, Seq] = findByPrimaryKeys(
    ts.map(t => DLCAnnouncementPrimaryKey(t.dlcId, t.announcementId))
  )

  def findByAnnouncementIds(
      ids: Vector[Long]
  ): Future[Vector[DLCAnnouncementDb]] = {
    val action = findByAnnouncementIdsAction(ids)
    safeDatabase.runVec(action)
  }

  def findByAnnouncementIdsAction(
      ids: Vector[Long]
  ): DBIOAction[Vector[DLCAnnouncementDb], NoStream, Effect.Read] = {
    val query = table.filter(_.announcementId.inSet(ids))
    query.result.map(_.toVector)
  }

  override def findByDLCIdAction(
      dlcId: Sha256Digest): DBIOAction[Vector[
                                         DLCAnnouncementDb
                                       ],
                                       profile.api.NoStream,
                                       profile.api.Effect.Read] = {
    val q = table.filter(_.dlcId === dlcId)
    q.result.map(_.toVector)
  }

  override def deleteByDLCIdAction(
      dlcId: Sha256Digest
  ): DBIOAction[Int, profile.api.NoStream, profile.api.Effect.Write] = {
    val q = table.filter(_.dlcId === dlcId)
    q.delete
  }

  class DLCAnnouncementTable(tag: Tag)
      extends Table[DLCAnnouncementDb](tag, schemaName, "dlc_announcements") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id")

    def announcementId: Rep[Long] = column("announcement_id")

    def index: Rep[Int] = column("index")

    def used: Rep[Boolean] = column("used")

    override def * : ProvenShape[DLCAnnouncementDb] =
      (dlcId, announcementId, index, used)
        .<>(DLCAnnouncementDb.apply, DLCAnnouncementDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(
        name = "pk_announcement_id_index",
        sourceColumns = (dlcId, announcementId)
      )

    def fkAnnouncementId: ForeignKeyQuery[?, OracleAnnouncementDataDb] =
      foreignKey(
        "fk_announcement_id",
        sourceColumns = announcementId,
        targetTableQuery = announcementDataTable
      )(_.id)

    def fkDLCId: ForeignKeyQuery[?, DLCDb] =
      foreignKey(
        "fk_dlc_id",
        sourceColumns = dlcId,
        targetTableQuery = dlcTable
      )(_.dlcId)
  }
}
