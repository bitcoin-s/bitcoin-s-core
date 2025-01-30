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

  private lazy val accountTable: TableQuery[Table[?]] = {
    AccountDAO()(ec, appConfig).table
  }

  private lazy val addressTable: TableQuery[Table[?]] = {
    AddressDAO()(ec, appConfig).table
  }

  private lazy val addressTagTable: TableQuery[Table[?]] = {
    AddressTagDAO()(ec, appConfig).table
  }

  private lazy val utxoTable: TableQuery[Table[?]] = {
    SpendingInfoDAO()(ec, appConfig).table
  }

  private lazy val txTable: TableQuery[Table[?]] = {
    TransactionDAO()(ec, appConfig).table
  }

  private lazy val incomingTxTable: TableQuery[Table[?]] = {
    IncomingTransactionDAO()(ec, appConfig).table
  }

  private lazy val outgoingTxTable: TableQuery[Table[?]] = {
    OutgoingTransactionDAO()(ec, appConfig).table
  }

  private lazy val spkTable: TableQuery[Table[?]] = {
    ScriptPubKeyDAO()(ec, appConfig).table
  }

  private lazy val stateDescriptorTable: TableQuery[Table[?]] = {
    WalletStateDescriptorDAO()(ec, appConfig).table
  }

  private lazy val masterXPubTable: TableQuery[Table[?]] = {
    MasterXPubDAO()(ec, appConfig).table
  }

  // Ordering matters here, tables with a foreign key should be listed after
  // the table that key references
  override lazy val allTables: List[TableQuery[Table[?]]] = {
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
