package org.bitcoins.node.db

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{BroadcastAbleTransactionDAO, PeerDAO}

import scala.concurrent.ExecutionContext

trait NodeDbManagement extends DbManagement {
  jdbcProfile: JdbcProfileComponent[NodeAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val txTable: TableQuery[Table[_]] = {
    BroadcastAbleTransactionDAO()(appConfig, ec).table
  }

  private lazy val peerTable: TableQuery[Table[_]] = {
    PeerDAO()(appConfig, ec).table
  }

  override lazy val allTables: List[TableQuery[Table[_]]] = {
    List(txTable, peerTable)
  }

}
