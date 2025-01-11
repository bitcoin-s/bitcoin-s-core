package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.db.RValueDb
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.crypto.SchnorrNonce
import org.bitcoins.db.{CRUD, DbCommonsColumnMappers, SlickUtil}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class RValueDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCOracleAppConfig
) extends CRUD[RValueDb, SchnorrNonce]
    with SlickUtil[RValueDb, SchnorrNonce] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[RValueTable] = TableQuery[RValueTable]

  override def createAll(ts: Vector[RValueDb]): Future[Vector[RValueDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[SchnorrNonce]
  ): Query[RValueTable, RValueDb, Seq] =
    table.filter(_.nonce.inSet(ids))

  override protected def findAll(
      ts: Vector[RValueDb]
  ): Query[RValueTable, RValueDb, Seq] =
    findByPrimaryKeys(ts.map(_.nonce))

  def findByNonce(nonce: SchnorrNonce): Future[Option[RValueDb]] = {
    findByNonces(Vector(nonce))
      .map(_.headOption)
  }

  def findByNonces(nonces: Vector[SchnorrNonce]): Future[Vector[RValueDb]] = {
    val action = table.filter(_.nonce.inSet(nonces)).result
    safeDatabase.runVec(action)
  }

  def maxKeyIndex: Future[Option[Int]] = {
    val query = table.map(_.keyIndex).max

    safeDatabase.run(query.result)
  }

  class RValueTable(tag: Tag)
      extends Table[RValueDb](tag, schemaName, "r_values") {

    def nonce: Rep[SchnorrNonce] = column("nonce", O.PrimaryKey)

    def eventName: Rep[String] = column("event_name", O.Unique)

    def purpose: Rep[HDPurpose] = column("hd_purpose")

    def coinType: Rep[HDCoinType] = column("coin")

    def accountIndex: Rep[Int] = column("account_index")

    def chainType: Rep[Int] = column("chain_type")

    def keyIndex: Rep[Int] = column("key_index", O.Unique)

    def * : ProvenShape[RValueDb] =
      (nonce, eventName, purpose, coinType, accountIndex, chainType, keyIndex)
        .<>(RValueDb.apply, RValueDb.unapply)
  }
}
