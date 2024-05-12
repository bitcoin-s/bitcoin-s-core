package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.wallet.db.TransactionDb
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.wallet.models.TxDAO
import slick.lifted.ProvenShape

import scala.concurrent.ExecutionContext

case class DLCRemoteTxDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends TxDAO[TransactionDb] {

  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table
      : slick.lifted.TableQuery[DLCRemoteTxDAO.this.DLCRemoteTxTable] =
    TableQuery[DLCRemoteTxTable]

  class DLCRemoteTxTable(tag: Tag)
      extends TxTable(tag, schemaName, "watch_only_tx_table") {
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
  }
}
