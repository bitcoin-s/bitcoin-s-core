package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{TransactionDb, TxDB}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

trait TxCRUDComponent[DbEntryType <: TxDB] {
  self: CRUD[DbEntryType, DoubleSha256DigestBE] =>
  import profile.api._

  abstract class TxTable(
      tag: profile.api.Tag,
      schemaName: Option[String],
      tableName: String
  ) extends Table[DbEntryType](tag, schemaName, tableName) {
    def txIdBE: Rep[DoubleSha256DigestBE]
  }
}

trait TxDAO[DbEntryType <: TxDB]
    extends CRUD[DbEntryType, DoubleSha256DigestBE]
    with TxCRUDComponent[DbEntryType]
    with SlickUtil[DbEntryType, DoubleSha256DigestBE] {
  import profile.api._
  implicit val ec: ExecutionContext

  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: TableQuery[? <: TxTable]

  override def createAll(ts: Vector[DbEntryType]): Future[Vector[DbEntryType]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findByPrimaryKeys(
      txIdBEs: Vector[DoubleSha256DigestBE]
  ): Query[TxTable, DbEntryType, Seq] =
    table
      .filter(_.txIdBE.inSet(txIdBEs))
      .asInstanceOf[Query[TxTable, DbEntryType, Seq]]

  override def findByPrimaryKey(
      txIdBE: DoubleSha256DigestBE
  ): Query[TxTable, DbEntryType, Seq] = {
    table
      .filter(_.txIdBE === txIdBE)
      .asInstanceOf[Query[TxTable, DbEntryType, Seq]]
  }

  override def findAll(
      txs: Vector[DbEntryType]
  ): Query[TxTable, DbEntryType, Seq] =
    findByPrimaryKeys(txs.map(_.txIdBE))

  def findByOutPoint(
      outPoint: TransactionOutPoint
  ): Future[Option[DbEntryType]] = {
    findByTxId(outPoint.txId)
  }

  def findByOutPoints(
      outPoints: Vector[TransactionOutPoint]
  ): Future[Vector[DbEntryType]] = {
    findByTxIds(outPoints.map(_.txIdBE))
  }

  def findByTxIds(
      txIdBEs: Vector[DoubleSha256DigestBE]
  ): Future[Vector[DbEntryType]] = {
    val q = findByPrimaryKeys(txIdBEs)

    safeDatabase.runVec(q.result)
  }

  def findByTxIdAction(
      txIdBE: DoubleSha256DigestBE
  ): DBIOAction[Option[DbEntryType], NoStream, Effect.Read] = {
    findByTxIdsAction(Vector(txIdBE)).map(_.headOption)
  }

  def findByTxIdsAction(
      txIdBEs: Vector[DoubleSha256DigestBE]
  ): DBIOAction[Vector[DbEntryType], NoStream, Effect.Read] = {
    table
      .filter(_.txIdBE.inSet(txIdBEs))
      .result
      .map(_.toVector)
  }

  def findByTxId(txIdBE: DoubleSha256DigestBE): Future[Option[DbEntryType]] = {
    safeDatabase.run(findByTxIdAction(txIdBE))
  }

  def findByTxId(txId: DoubleSha256Digest): Future[Option[DbEntryType]] =
    findByTxId(txId.flip)

  def findByTxIdBEs(
      txIdBEs: Vector[DoubleSha256DigestBE]
  ): Future[Vector[DbEntryType]] = {
    safeDatabase.run(findByPrimaryKeys(txIdBEs).result).map(_.toVector)
  }
}

case class TransactionDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: WalletAppConfig
) extends TxDAO[TransactionDb] {

  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: slick.lifted.TableQuery[TransactionTable] =
    TableQuery[TransactionTable]

  def findAllUnconfirmed(): Future[Vector[TransactionDb]] = {
    val query = table.filter(_.blockHash === Rep.None[DoubleSha256DigestBE])

    safeDatabase.runVec(query.result)
  }

  def findAllConfirmed(): Future[Vector[TransactionDb]] = {
    val query = table.filterNot(_.blockHash === Rep.None[DoubleSha256DigestBE])

    safeDatabase.runVec(query.result)
  }

  class TransactionTable(tag: Tag)
      extends TxTable(tag, schemaName, "tx_table") {

    def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.PrimaryKey)

    def transaction: Rep[Transaction] = column("transaction")

    def unsignedTxIdBE: Rep[DoubleSha256DigestBE] = column("unsignedTxIdBE")

    def unsignedTx: Rep[Transaction] = column("unsignedTx")

    def wTxIdBEOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("wTxIdBE")

    def totalOutput: Rep[CurrencyUnit] = column("totalOutput")

    def numInputs: Rep[Int] = column("numInputs")

    def numOutputs: Rep[Int] = column("numOutputs")

    def locktime: Rep[UInt32] = column("locktime")

    def blockHash: Rep[Option[DoubleSha256DigestBE]] = column("block_hash")

    def * : ProvenShape[TransactionDb] =
      (
        txIdBE,
        transaction,
        unsignedTxIdBE,
        unsignedTx,
        wTxIdBEOpt,
        totalOutput,
        numInputs,
        numOutputs,
        locktime,
        blockHash
      ).<>(TransactionDb.apply, TransactionDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey("pk_tx", sourceColumns = txIdBE)
  }
}
