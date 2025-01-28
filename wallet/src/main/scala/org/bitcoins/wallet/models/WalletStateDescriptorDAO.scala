package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.WalletStateDescriptorType.{
  Rescan,
  SyncHeight
}
import org.bitcoins.core.api.wallet.{
  RescanDescriptor,
  SyncHeightDescriptor,
  WalletStateDescriptor,
  WalletStateDescriptorType
}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class WalletStateDescriptorDb(
    tpe: WalletStateDescriptorType,
    descriptor: WalletStateDescriptor
) {
  require(descriptor.descriptorType == tpe)
}

case class WalletStateDescriptorDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: WalletAppConfig
) extends CRUD[WalletStateDescriptorDb, WalletStateDescriptorType]
    with SlickUtil[WalletStateDescriptorDb, WalletStateDescriptorType] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[WalletStateDescriptorTable] =
    TableQuery[WalletStateDescriptorTable]

  override def createAll(
      ts: Vector[WalletStateDescriptorDb]
  ): Future[Vector[WalletStateDescriptorDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findByPrimaryKeys(
      ids: Vector[WalletStateDescriptorType]
  ): Query[WalletStateDescriptorTable, WalletStateDescriptorDb, Seq] = {
    table.filter(_.tpe.inSet(ids))
  }

  override def findByPrimaryKey(
      id: WalletStateDescriptorType
  ): Query[Table[WalletStateDescriptorDb], WalletStateDescriptorDb, Seq] = {
    table.filter(_.tpe === id)
  }

  override def findAll(
      ts: Vector[WalletStateDescriptorDb]
  ): Query[Table[WalletStateDescriptorDb], WalletStateDescriptorDb, Seq] =
    findByPrimaryKeys(ts.map(_.tpe))

  def getSyncHeight(): Future[Option[SyncHeightDescriptor]] = {
    read(SyncHeight).map {
      case Some(db) =>
        val desc = SyncHeightDescriptor.fromString(db.descriptor.toString)
        Some(desc)
      case None => None
    }
  }

  def updateSyncHeight(
      hash: DoubleSha256DigestBE,
      height: Int
  ): Future[WalletStateDescriptorDb] = {
    val tpe: WalletStateDescriptorType = SyncHeight
    val query = table.filter(_.tpe === tpe)
    val action = for {
      oldOpt <- query.result.headOption
      res: WalletStateDescriptorDb <- oldOpt match {
        case Some(oldDb) =>
          val old = SyncHeightDescriptor.fromString(oldDb.descriptor.toString)
          if (old.height > height) {
            DBIO.successful(WalletStateDescriptorDb(tpe, old))
          } else {
            val descriptor = SyncHeightDescriptor(hash, height)
            val newDb = WalletStateDescriptorDb(tpe, descriptor)
            query.update(newDb).map(_ => newDb)
          }
        case None =>
          val descriptor = SyncHeightDescriptor(hash, height)
          val db = WalletStateDescriptorDb(tpe, descriptor)
          (table += db).map(_ => db)
      }
    } yield res
    safeDatabase.run(action)
  }

  def getRescan(): Future[Option[RescanDescriptor]] = {
    read(Rescan).map {
      case Some(db) =>
        val desc = RescanDescriptor.fromString(db.descriptor.toString)
        Some(desc)
      case None => None
    }
  }

  def isRescanning: Future[Boolean] = getRescan().map(_.exists(_.rescanning))

  def updateRescanning(rescanning: Boolean): Future[RescanDescriptor] = {
    val desc = RescanDescriptor(rescanning)
    upsert(WalletStateDescriptorDb(desc.descriptorType, desc)).map(_ => desc)
  }

  def compareAndSetRescanning(
      expectedValue: Boolean,
      newValue: Boolean
  ): Future[Boolean] = {
    val tpe: WalletStateDescriptorType = Rescan
    val query = table.filter(_.tpe === tpe)

    val actions = for {
      dbs <- query.result
      res <- dbs.headOption match {
        case None =>
          val desc = RescanDescriptor(newValue)
          val db = WalletStateDescriptorDb(tpe, desc)
          (table += db).map(_ => true)
        case Some(db) =>
          val oldDesc = RescanDescriptor.fromString(db.descriptor.toString)
          if (oldDesc.rescanning == expectedValue) {
            val newDesc = RescanDescriptor(true)
            val newDb = WalletStateDescriptorDb(tpe, newDesc)
            query.update(newDb).map(_ => true)
          } else {
            DBIO.successful(false)
          }
      }
    } yield res

    safeDatabase.run(actions)
  }

  class WalletStateDescriptorTable(t: Tag)
      extends Table[WalletStateDescriptorDb](
        t,
        schemaName,
        "state_descriptors"
      ) {

    def tpe: Rep[WalletStateDescriptorType] = column("type", O.PrimaryKey)

    def descriptor: Rep[WalletStateDescriptor] = column("descriptor")

    override def * : ProvenShape[WalletStateDescriptorDb] =
      (tpe, descriptor).<>(
        WalletStateDescriptorDb.apply,
        WalletStateDescriptorDb.unapply
      )

  }
}
