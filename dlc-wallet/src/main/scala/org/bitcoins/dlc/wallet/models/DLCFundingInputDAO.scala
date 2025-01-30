package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCFundingInputDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[DLCFundingInputDb, TransactionOutPoint]
    with SlickUtil[DLCFundingInputDb, TransactionOutPoint]
    with DLCIdDaoUtilNoPK[DLCFundingInputDb] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCFundingInputsTable] =
    TableQuery[DLCFundingInputsTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCFundingInputDb]
  ): Future[Vector[DLCFundingInputDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      outPoints: Vector[TransactionOutPoint]
  ): Query[DLCFundingInputsTable, DLCFundingInputDb, Seq] =
    table.filter(_.outPoint.inSet(outPoints))

  override def findByPrimaryKey(
      outPoint: TransactionOutPoint
  ): Query[DLCFundingInputsTable, DLCFundingInputDb, Seq] = {
    table
      .filter(_.outPoint === outPoint)
  }

  override def findAll(
      dlcs: Vector[DLCFundingInputDb]
  ): Query[DLCFundingInputsTable, DLCFundingInputDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.outPoint))

  override def findByDLCIdAction(
      dlcId: Sha256Digest): DBIOAction[Vector[
                                         DLCFundingInputDb
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

  def findByDLCId(
      dlcId: Sha256Digest,
      isInitiator: Boolean
  ): Future[Vector[DLCFundingInputDb]] = {
    val q = table
      .filter(_.dlcId === dlcId)
      .filter(_.isInitiator === isInitiator)

    safeDatabase.run(q.result).map(_.toVector)
  }

  class DLCFundingInputsTable(tag: Tag)
      extends Table[DLCFundingInputDb](tag, schemaName, "funding_inputs") {

    def outPoint: Rep[TransactionOutPoint] = column("out_point", O.PrimaryKey)

    def dlcId: Rep[Sha256Digest] = column("dlc_id")

    def inputSerialId: Rep[UInt64] = column("input_serial_id")

    def index: Rep[Int] = column("index")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def output: Rep[TransactionOutput] = column("output")

    def nSequence: Rep[UInt32] = column("sequence")

    def maxWitnessLength: Rep[Long] = column("max_witness_length")

    def redeemScriptOpt: Rep[Option[ScriptPubKey]] = column("redeem_script_opt")

    def witnessScriptOpt: Rep[Option[ScriptWitness]] =
      column("witness_script_opt")

    def * : ProvenShape[DLCFundingInputDb] =
      (
        dlcId,
        isInitiator,
        index,
        inputSerialId,
        outPoint,
        output,
        nSequence,
        maxWitnessLength,
        redeemScriptOpt,
        witnessScriptOpt
      ).<>(DLCFundingInputDb.apply, DLCFundingInputDb.unapply)

    def fk: ForeignKeyQuery[?, DLCDb] =
      foreignKey(
        "fk_dlc_id",
        sourceColumns = dlcId,
        targetTableQuery = dlcTable
      )(_.dlcId)
  }
}
