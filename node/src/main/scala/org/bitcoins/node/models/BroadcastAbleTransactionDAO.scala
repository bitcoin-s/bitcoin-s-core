package org.bitcoins.node.models

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.node.config.NodeAppConfig
import scodec.bits.ByteVector
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class BroadcastAbleTransactionDAO()(implicit
    override val appConfig: NodeAppConfig,
    override val ec: ExecutionContext)
    extends CRUD[BroadcastAbleTransaction, DoubleSha256DigestBE]
    with SlickUtil[BroadcastAbleTransaction, DoubleSha256DigestBE] {

  import profile.api._
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[BroadcastAbleTransactionTable] =
    profile.api.TableQuery[BroadcastAbleTransactionTable]

  override def createAll(ts: Vector[BroadcastAbleTransaction]): Future[
    Vector[BroadcastAbleTransaction]] = createAllNoAutoInc(ts, safeDatabase)

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      txIds: Vector[DoubleSha256DigestBE]): Query[
    BroadcastAbleTransactionTable,
    BroadcastAbleTransaction,
    Seq] = {
    table.filter(_.txid.inSet(txIds))
  }

  override protected def findAll(ts: Vector[BroadcastAbleTransaction]): Query[
    BroadcastAbleTransactionTable,
    BroadcastAbleTransaction,
    Seq] = findByPrimaryKeys(ts.map(_.transaction.txIdBE))

  def findByHash(
      hash: DoubleSha256DigestBE): Future[Option[BroadcastAbleTransaction]] =
    findByHash(hash.flip)

  /** Searches for a TX by its TXID */
  def findByHash(
      hash: DoubleSha256Digest): Future[Option[BroadcastAbleTransaction]] = {
    val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
    import mappers._

    val query = table.filter(_.txid === hash.flip)
    safeDatabase.run(query.result).map(_.headOption)
  }

  /** Table over TXs we can broadcast over the P2P network */
  class BroadcastAbleTransactionTable(tag: Tag)
      extends Table[BroadcastAbleTransaction](tag,
                                              schemaName,
                                              "broadcast_elements") {
    private type Tuple = (DoubleSha256DigestBE, ByteVector)

    private val fromTuple: Tuple => BroadcastAbleTransaction = {
      case (txid, bytes) =>
        val tx = Transaction.fromBytes(bytes)
        require(tx.txId == txid.flip)
        BroadcastAbleTransaction(tx)
    }

    private val toTuple: BroadcastAbleTransaction => Option[Tuple] = tx =>
      Some((tx.transaction.txId.flip, tx.transaction.bytes))

    def txid: Rep[DoubleSha256DigestBE] = column("txid", O.PrimaryKey)
    def bytes: Rep[ByteVector] = column("tx_bytes")

    def * : ProvenShape[BroadcastAbleTransaction] =
      (txid, bytes).<>(fromTuple, toTuple)
  }
}
