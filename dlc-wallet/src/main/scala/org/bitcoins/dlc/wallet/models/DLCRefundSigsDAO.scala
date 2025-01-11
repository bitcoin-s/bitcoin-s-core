package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{ECDigitalSignature, Sha256Digest}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCRefundSigsDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[DLCRefundSigsDb, Sha256Digest]
    with SlickUtil[DLCRefundSigsDb, Sha256Digest]
    with DLCIdDaoUtil[DLCRefundSigsDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCRefundSigTable] =
    TableQuery[DLCRefundSigTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCRefundSigsDb]
  ): Future[Vector[DLCRefundSigsDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256Digest]
  ): Query[DLCRefundSigTable, DLCRefundSigsDb, Seq] =
    table.filter(_.dlcId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256Digest
  ): Query[DLCRefundSigTable, DLCRefundSigsDb, Seq] = {
    table
      .filter(_.dlcId === id)
  }

  override def findAll(
      dlcs: Vector[DLCRefundSigsDb]
  ): Query[DLCRefundSigTable, DLCRefundSigsDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.dlcId))

  override def findByDLCIdsAction(
      dlcIds: Vector[Sha256Digest]
  ): DBIOAction[Vector[
                  DLCRefundSigsDb
                ],
                profile.api.NoStream,
                profile.api.Effect.Read] = {
    val q = table.filter(_.dlcId.inSet(dlcIds))
    q.result.map(_.toVector)
  }

  override def deleteByDLCIdAction(
      dlcId: Sha256Digest
  ): DBIOAction[Int, profile.api.NoStream, profile.api.Effect.Write] = {
    val q = table.filter(_.dlcId === dlcId)
    q.delete
  }

  class DLCRefundSigTable(tag: Tag)
      extends Table[DLCRefundSigsDb](tag, schemaName, "refund_sigs") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id", O.PrimaryKey)

    def accepterSig: Rep[PartialSignature[ECDigitalSignature]] = column(
      "accepter_sig")

    def initiatorSig: Rep[Option[PartialSignature[ECDigitalSignature]]] =
      column("initiator_sig")

    def * : ProvenShape[DLCRefundSigsDb] =
      (dlcId, accepterSig, initiatorSig).<>(
        DLCRefundSigsDb.apply,
        DLCRefundSigsDb.unapply
      )

    def fk: ForeignKeyQuery[?, DLCDb] =
      foreignKey(
        "fk_dlc_id",
        sourceColumns = dlcId,
        targetTableQuery = dlcTable
      )(_.dlcId)
  }
}
