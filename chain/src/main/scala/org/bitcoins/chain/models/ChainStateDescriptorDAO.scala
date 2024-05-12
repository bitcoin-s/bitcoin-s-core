package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.ChainStateDescriptorType.Syncing
import org.bitcoins.db.{CRUD, SlickUtil}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class ChainStateDescriptorDb(
    tpe: ChainStateDescriptorType,
    descriptor: ChainStateDescriptor
) {
  require(
    descriptor.descriptorType == tpe,
    s"descriptorTpe=${descriptor.descriptorType} tpe=$tpe"
  )
}

case class ChainStateDescriptorDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: ChainAppConfig
) extends CRUD[ChainStateDescriptorDb, ChainStateDescriptorType]
    with SlickUtil[ChainStateDescriptorDb, ChainStateDescriptorType] {
  import profile.api._

  implicit val chainStateDescriptorTypeMapper
      : BaseColumnType[ChainStateDescriptorType] =
    MappedColumnType.base[ChainStateDescriptorType, String](
      _.toString,
      ChainStateDescriptorType.fromString
    )

  implicit val chainStateDescriptorMapper
      : BaseColumnType[ChainStateDescriptor] =
    MappedColumnType.base[ChainStateDescriptor, String](
      _.toString,
      ChainStateDescriptor.fromString
    )

  override val table: profile.api.TableQuery[ChainStateDescriptorTable] =
    TableQuery[ChainStateDescriptorTable]

  override def createAll(
      ts: Vector[ChainStateDescriptorDb]
  ): Future[Vector[ChainStateDescriptorDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findByPrimaryKeys(
      ids: Vector[ChainStateDescriptorType]
  ): Query[ChainStateDescriptorTable, ChainStateDescriptorDb, Seq] = {
    table.filter(_.tpe.inSet(ids))
  }

  override def findByPrimaryKey(
      id: ChainStateDescriptorType
  ): Query[Table[ChainStateDescriptorDb], ChainStateDescriptorDb, Seq] = {
    table.filter(_.tpe === id)
  }

  override def findAll(
      ts: Vector[ChainStateDescriptorDb]
  ): Query[Table[ChainStateDescriptorDb], ChainStateDescriptorDb, Seq] =
    findByPrimaryKeys(ts.map(_.tpe))

  def getSync(): Future[Option[SyncDescriptor]] = {
    read(Syncing).map {
      case Some(db) =>
        val desc = SyncDescriptor.fromString(db.descriptor.toString)
        Some(desc)
      case None => None
    }
  }

  def getIsIBD(): Future[Option[IsInitialBlockDownload]] = {
    read(IsInitialBlockDownload.tpe).map {
      case Some(db) =>
        val desc = IsInitialBlockDownload.fromString(db.descriptor.toString)
        Some(desc)
      case None => None
    }
  }

  def isSyncing: Future[Boolean] = getSync().map(_.exists(_.syncing))

  def updateSyncing(syncing: Boolean): Future[Boolean] = {
    val tpe: ChainStateDescriptorType = Syncing
    val query = table.filter(_.tpe === tpe)
    val actions = for {
      dbs <- query.result
      res <- dbs.headOption match {
        case None =>
          val desc = SyncDescriptor(syncing)
          val db = ChainStateDescriptorDb(tpe, desc)
          (table += db).map(_ => syncing)
        case Some(db) =>
          val oldDesc = SyncDescriptor.fromString(db.descriptor.toString)
          if (oldDesc.syncing != syncing) {
            val newDesc = SyncDescriptor(syncing)
            val newDb = ChainStateDescriptorDb(tpe, newDesc)
            query.update(newDb).map(_ => true)
          } else {
            DBIO.successful(false)
          }
      }
    } yield res

    safeDatabase.run(actions)
  }

  def updateIsIbd(isIBDRunning: Boolean): Future[Boolean] = {
    val tpe: ChainStateDescriptorType = IsInitialBlockDownload.tpe
    val query = table.filter(_.tpe === tpe)
    val actions = for {
      dbs <- query.result
      res <- dbs.headOption match {
        case None =>
          val desc = IsInitialBlockDownload(isIBDRunning)
          val db = ChainStateDescriptorDb(tpe, desc)
          (table += db).map(_ => isIBDRunning)
        case Some(db) =>
          val oldDesc =
            IsInitialBlockDownload.fromString(db.descriptor.toString)
          if (oldDesc.isIBDRunning) {
            val newDesc = IsInitialBlockDownload(isIBDRunning)
            val newDb = ChainStateDescriptorDb(tpe, newDesc)
            query.update(newDb).map(_ => true)
          } else {
            DBIO.successful(false)
          }
      }
    } yield res

    safeDatabase.run(actions)
  }

  class ChainStateDescriptorTable(t: Tag)
      extends Table[ChainStateDescriptorDb](
        t,
        schemaName,
        "state_descriptors"
      ) {

    def tpe: Rep[ChainStateDescriptorType] = column("type", O.PrimaryKey)

    def descriptor: Rep[ChainStateDescriptor] = column("descriptor")

    override def * : ProvenShape[ChainStateDescriptorDb] =
      (tpe, descriptor).<>(
        ChainStateDescriptorDb.apply,
        ChainStateDescriptorDb.unapply
      )

  }
}
