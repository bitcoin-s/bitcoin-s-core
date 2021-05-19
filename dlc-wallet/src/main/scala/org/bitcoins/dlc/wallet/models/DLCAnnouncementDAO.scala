package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class DLCAnnouncementDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCAnnouncementDb, (Sha256Digest, Long)]
    with SlickUtil[DLCAnnouncementDb, (Sha256Digest, Long)] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCAnnouncementTable] =
    TableQuery[DLCAnnouncementTable]

  private lazy val announcementDataTable: slick.lifted.TableQuery[
    OracleAnnouncementDataDAO#OracleAnnouncementsTable] = {
    OracleAnnouncementDataDAO().table
  }

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCAnnouncementDb]): Future[Vector[DLCAnnouncementDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[(Sha256Digest, Long)]): profile.api.Query[
    DLCAnnouncementTable,
    DLCAnnouncementDb,
    Seq] = {

    // is there a better way to do this?
    val starting = table.filterNot(_.dlcId === Sha256Digest.empty)

    ids.foldLeft(starting) { case (accum, (dlcId, announcementId)) =>
      accum.flatMap(_ =>
        table.filter(t =>
          t.dlcId === dlcId &&
            t.announcementId === announcementId))
    }
  }

  override def findByPrimaryKey(id: (Sha256Digest, Long)): Query[
    DLCAnnouncementTable,
    DLCAnnouncementDb,
    Seq] = {
    table.filter(t => t.dlcId === id._1 && t.announcementId === id._2)
  }

  override def find(t: DLCAnnouncementDb): profile.api.Query[
    Table[_],
    DLCAnnouncementDb,
    Seq] = {
    findByPrimaryKey((t.dlcId, t.announcementId))
  }

  override protected def findAll(ts: Vector[DLCAnnouncementDb]): Query[
    DLCAnnouncementTable,
    DLCAnnouncementDb,
    Seq] = findByPrimaryKeys(ts.map(t => (t.dlcId, t.announcementId)))

  def findByAnnouncementIds(
      ids: Vector[Long]): Future[Vector[DLCAnnouncementDb]] = {
    val query = table.filter(_.announcementId.inSet(ids))

    safeDatabase.runVec(query.result)
  }

  def findByDLCId(dlcId: Sha256Digest): Future[Vector[DLCAnnouncementDb]] = {
    val query = table.filter(_.dlcId === dlcId)

    safeDatabase.runVec(query.result)
  }

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.run(q.delete)
  }

  class DLCAnnouncementTable(tag: Tag)
      extends Table[DLCAnnouncementDb](tag, schemaName, "dlc_announcements") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id")

    def announcementId: Rep[Long] = column("announcement_id")

    def index: Rep[Int] = column("index")

    def used: Rep[Boolean] = column("used")

    override def * : ProvenShape[DLCAnnouncementDb] =
      (dlcId, announcementId, index, used)
        .<>(DLCAnnouncementDb.tupled, DLCAnnouncementDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_announcement_id_index",
                 sourceColumns = (dlcId, announcementId))

    def fkAnnouncementId: ForeignKeyQuery[_, OracleAnnouncementDataDb] =
      foreignKey("fk_announcement_id",
                 sourceColumns = announcementId,
                 targetTableQuery = announcementDataTable)(_.id)

    def fkDLCId: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_dlc_id",
                 sourceColumns = dlcId,
                 targetTableQuery = dlcTable)(_.dlcId)
  }
}
