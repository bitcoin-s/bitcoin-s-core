package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{IncomingTransactionDb, TransactionDb}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.wallet.config._
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.ExecutionContext

case class IncomingTransactionDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: WalletAppConfig
) extends TxDAO[IncomingTransactionDb] {
  import profile.api._

  override val table: slick.lifted.TableQuery[
    IncomingTransactionDAO.this.IncomingTransactionTable] = {
    TableQuery[IncomingTransactionTable]
  }

  private lazy val txTable
      : profile.api.TableQuery[TransactionDAO#TransactionTable] = {
    TransactionDAO().table
  }

  class IncomingTransactionTable(tag: Tag)
      extends TxTable(
        tag,
        schemaName,
        "wallet_incoming_txs"
      ) {

    private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
    import mappers._

    def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

    def incomingAmount: Rep[CurrencyUnit] = column("incomingAmount")

    private type IncomingTransactionTuple = (DoubleSha256DigestBE, CurrencyUnit)

    private val fromTuple: IncomingTransactionTuple => IncomingTransactionDb = {
      case (txId, incomingAmount) =>
        IncomingTransactionDb(txId, incomingAmount)
    }

    private val toTuple
        : IncomingTransactionDb => Option[IncomingTransactionTuple] = tx =>
      Some((tx.txIdBE, tx.incomingAmount))

    def * : ProvenShape[IncomingTransactionDb] =
      (txIdBE, incomingAmount).<>(fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey("pk_in_tx", sourceColumns = txIdBE)

    def fk_underlying_tx: slick.lifted.ForeignKeyQuery[?, TransactionDb] = {
      foreignKey(
        "fk_underlying_tx",
        sourceColumns = txIdBE,
        targetTableQuery = txTable
      )(_.txIdBE)
    }
  }
}
