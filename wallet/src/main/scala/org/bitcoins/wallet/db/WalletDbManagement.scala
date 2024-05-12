package org.bitcoins.wallet.db

import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._

import scala.concurrent.ExecutionContext

trait WalletDbManagement extends DbManagement {
  this: JdbcProfileComponent[WalletAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val accountTable: TableQuery[Table[_]] = {
    AccountDAO()(ec, appConfig).table
  }

  private lazy val addressTable: TableQuery[Table[_]] = {
    AddressDAO()(ec, appConfig).table
  }

  private lazy val addressTagTable: TableQuery[Table[_]] = {
    AddressTagDAO()(ec, appConfig).table
  }

  private lazy val utxoTable: TableQuery[Table[_]] = {
    SpendingInfoDAO()(ec, appConfig).table
  }

  private lazy val txTable: TableQuery[Table[_]] = {
    TransactionDAO()(ec, appConfig).table
  }

  private lazy val incomingTxTable: TableQuery[Table[_]] = {
    IncomingTransactionDAO()(ec, appConfig).table
  }

  private lazy val outgoingTxTable: TableQuery[Table[_]] = {
    OutgoingTransactionDAO()(ec, appConfig).table
  }

  private lazy val spkTable: TableQuery[Table[_]] = {
    ScriptPubKeyDAO()(ec, appConfig).table
  }

  private lazy val stateDescriptorTable: TableQuery[Table[_]] = {
    WalletStateDescriptorDAO()(ec, appConfig).table
  }

  private lazy val masterXPubTable: TableQuery[Table[_]] = {
    MasterXPubDAO()(ec, appConfig).table
  }

  // Ordering matters here, tables with a foreign key should be listed after
  // the table that key references
  override lazy val allTables: List[TableQuery[Table[_]]] = {
    List(
      spkTable,
      accountTable,
      addressTable,
      addressTagTable,
      txTable,
      incomingTxTable,
      utxoTable,
      outgoingTxTable,
      stateDescriptorTable,
      masterXPubTable
    )
  }

}
