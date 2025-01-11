package org.bitcoins.node.models

import org.bitcoins.core.api.node.NodeStateDescriptorType.WalletName
import org.bitcoins.core.api.node.{
  NodeStateDescriptor,
  NodeStateDescriptorType,
  WalletNameDescriptor
}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.node.config.NodeAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class NodeStateDescriptorDb(
    tpe: NodeStateDescriptorType,
    descriptor: NodeStateDescriptor
) {
  require(descriptor.descriptorType == tpe)
}

case class NodeStateDescriptorDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: NodeAppConfig
) extends CRUD[NodeStateDescriptorDb, NodeStateDescriptorType]
    with SlickUtil[NodeStateDescriptorDb, NodeStateDescriptorType] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[NodeStateDescriptorTable] =
    TableQuery[NodeStateDescriptorTable]

  override def createAll(
      ts: Vector[NodeStateDescriptorDb]
  ): Future[Vector[NodeStateDescriptorDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findByPrimaryKeys(
      ids: Vector[NodeStateDescriptorType]
  ): Query[NodeStateDescriptorTable, NodeStateDescriptorDb, Seq] = {
    table.filter(_.tpe.inSet(ids))
  }

  override def findByPrimaryKey(
      id: NodeStateDescriptorType
  ): Query[Table[NodeStateDescriptorDb], NodeStateDescriptorDb, Seq] = {
    table.filter(_.tpe === id)
  }

  override def findAll(
      ts: Vector[NodeStateDescriptorDb]
  ): Query[Table[NodeStateDescriptorDb], NodeStateDescriptorDb, Seq] =
    findByPrimaryKeys(ts.map(_.tpe))

//  def setWalletName(walletName: Option[String]): Future[Unit] = {
//
//  }

  def getWalletName(): Future[Option[WalletNameDescriptor]] = {
    read(WalletName).map {
      case Some(db) =>
        val desc = WalletNameDescriptor.fromString(db.descriptor.toString)
        Some(desc)
      case None => None
    }
  }

  def updateWalletName(walletNameOpt: Option[String]): Future[Unit] = {
    val tpe: NodeStateDescriptorType = WalletName
    walletNameOpt match {
      case Some(walletName) =>
        val descriptor = WalletNameDescriptor(walletName)
        val newDb = NodeStateDescriptorDb(tpe, descriptor)
        upsert(newDb).map(_ => ())
      case None =>
        val query = table.filter(_.tpe === tpe)
        safeDatabase.run(query.delete).map(_ => ())
    }
  }

  class NodeStateDescriptorTable(t: Tag)
      extends Table[NodeStateDescriptorDb](t, schemaName, "state_descriptors") {

    def tpe: Rep[NodeStateDescriptorType] = column("type", O.PrimaryKey)

    def descriptor: Rep[NodeStateDescriptor] = column("descriptor")

    override def * : ProvenShape[NodeStateDescriptorDb] =
      (tpe, descriptor).<>(
        NodeStateDescriptorDb.apply,
        NodeStateDescriptorDb.unapply
      )

  }
}
