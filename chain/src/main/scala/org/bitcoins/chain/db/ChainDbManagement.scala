package org.bitcoins.chain.db

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  ChainStateDescriptorDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.db.{DbManagement, JdbcProfileComponent}

import scala.concurrent.ExecutionContext

/** Responsible for creating and destroying database tables inside of the Chain
  * project.
  */
trait ChainDbManagement extends DbManagement {
  this: JdbcProfileComponent[ChainAppConfig] =>
  import profile.api._

  def ec: ExecutionContext

  private lazy val chainTable: TableQuery[Table[?]] =
    BlockHeaderDAO()(ec, appConfig).table

  private lazy val filterHeaderTable: TableQuery[Table[?]] = {
    CompactFilterHeaderDAO()(ec, appConfig).table
  }

  private lazy val filterTable: TableQuery[Table[?]] = {
    CompactFilterDAO()(ec, appConfig).table
  }

  private lazy val stateTable: TableQuery[Table[?]] = {
    ChainStateDescriptorDAO()(ec, appConfig).table
  }

  override lazy val allTables: List[TableQuery[Table[?]]] =
    List(chainTable, filterHeaderTable, filterTable, stateTable)
}
