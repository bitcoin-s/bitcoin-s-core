package org.bitcoins.dlc.wallet

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.dlc.wallet.models._

import scala.concurrent.ExecutionContext

trait DLCDbManagement extends DbManagement {
  self: JdbcProfileComponent[DLCAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val announcementTable: TableQuery[Table[?]] = {
    OracleAnnouncementDataDAO()(ec, appConfig).table
  }

  private lazy val nonceTable: TableQuery[Table[?]] = {
    OracleNonceDAO()(ec, appConfig).table
  }

  private lazy val dlcTable: TableQuery[Table[?]] = {
    DLCDAO()(ec, appConfig).table
  }

  private lazy val contractDataTable: TableQuery[Table[?]] = {
    DLCContractDataDAO()(ec, appConfig).table
  }

  private lazy val dlcAnnouncementTable: TableQuery[Table[?]] = {
    DLCAnnouncementDAO()(ec, appConfig).table
  }

  private lazy val dlcOfferTable: TableQuery[Table[?]] = {
    DLCOfferDAO()(ec, appConfig).table
  }

  private lazy val dlcAcceptTable: TableQuery[Table[?]] = {
    DLCAcceptDAO()(ec, appConfig).table
  }

  private lazy val dlcFundingInputsTable: TableQuery[Table[?]] = {
    DLCFundingInputDAO()(ec, appConfig).table
  }

  private lazy val dlcCETSigTable: TableQuery[Table[?]] = {
    DLCCETSignaturesDAO()(ec, appConfig).table
  }

  private lazy val dlcRefundSigTable: TableQuery[Table[?]] = {
    DLCRefundSigsDAO()(ec, appConfig).table
  }

  private lazy val dlcRemoteTxTable: TableQuery[Table[?]] = {
    DLCRemoteTxDAO()(ec, appConfig).table
  }

  // Ordering matters here, tables with a foreign key should be listed after
  // the table that key references
  override lazy val allTables: List[TableQuery[Table[?]]] = {
    List(
      announcementTable,
      nonceTable,
      dlcTable,
      contractDataTable,
      dlcAnnouncementTable,
      dlcOfferTable,
      dlcAcceptTable,
      dlcFundingInputsTable,
      dlcCETSigTable,
      dlcRefundSigTable,
      dlcRemoteTxTable
    )
  }

}
