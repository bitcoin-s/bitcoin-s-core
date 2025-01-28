package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignaturesPrimaryKey(dlcId: Sha256Digest, contractIndex: Long)

case class DLCCETSignaturesDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[DLCCETSignaturesDb, DLCCETSignaturesPrimaryKey]
    with SlickUtil[DLCCETSignaturesDb, DLCCETSignaturesPrimaryKey]
    with DLCIdDaoUtilNoPK[DLCCETSignaturesDb] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCCETSignatureTable] =
    TableQuery[DLCCETSignatureTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCCETSignaturesDb]
  ): Future[Vector[DLCCETSignaturesDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[DLCCETSignaturesPrimaryKey]
  ): Query[DLCCETSignatureTable, DLCCETSignaturesDb, Seq] = {
    // is there a better way to do this?
    val starting = table.filter(_.dlcId =!= Sha256Digest.empty)

    val group = ids.groupBy(_.dlcId)

    group
      .foldLeft(starting) { case (accum, (dlcId, vec)) =>
        accum.flatMap { _ =>
          table.filter(t =>
            t.dlcId === dlcId && t.index.inSet(vec.map(_.contractIndex)))
        }
      }
  }

  override def findByPrimaryKey(
      id: DLCCETSignaturesPrimaryKey
  ): Query[DLCCETSignatureTable, DLCCETSignaturesDb, Seq] = {
    table.filter(t => t.dlcId === id.dlcId && t.index === id.contractIndex)
  }

  override def find(
      t: DLCCETSignaturesDb
  ): Query[DLCCETSignatureTable, DLCCETSignaturesDb, Seq] = {
    findByPrimaryKey(DLCCETSignaturesPrimaryKey(t.dlcId, t.index))
  }

  override def findAll(
      dlcs: Vector[DLCCETSignaturesDb]
  ): Query[DLCCETSignatureTable, DLCCETSignaturesDb, Seq] =
    findByPrimaryKeys(
      dlcs.map(sig => DLCCETSignaturesPrimaryKey(sig.dlcId, sig.index))
    )

  override def findByDLCIdAction(
      dlcId: Sha256Digest): DBIOAction[Vector[
                                         DLCCETSignaturesDb
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

  class DLCCETSignatureTable(tag: Tag)
      extends Table[DLCCETSignaturesDb](tag, schemaName, "cet_sigs") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id")

    def index: Rep[Long] = column("index")

    def sigPoint: Rep[ECPublicKey] = column("sig_point")

    def accepterSig: Rep[ECAdaptorSignature] = column("accepter_sig")

    def initiatorSig: Rep[Option[ECAdaptorSignature]] = column("initiator_sig")

    def * : ProvenShape[DLCCETSignaturesDb] =
      (dlcId, index, sigPoint, accepterSig, initiatorSig).<>(
        DLCCETSignaturesDb.apply,
        DLCCETSignaturesDb.unapply
      )

    def pk: PrimaryKey =
      primaryKey(name = "pk_cet_sigs", sourceColumns = (dlcId, index))

    def fk =
      foreignKey(
        "fk_dlc_id",
        sourceColumns = dlcId,
        targetTableQuery = dlcTable
      )(_.dlcId)
  }
}
