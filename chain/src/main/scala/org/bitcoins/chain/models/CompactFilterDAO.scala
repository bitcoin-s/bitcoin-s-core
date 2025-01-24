package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.db.CompactFilterDb
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.db.{CRUD, SlickUtil}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

case class CompactFilterDAO()(implicit
    ec: ExecutionContext,
    override val appConfig: ChainAppConfig
) extends CRUD[CompactFilterDb, DoubleSha256DigestBE]
    with SlickUtil[CompactFilterDb, DoubleSha256DigestBE] {
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers.{
    byteVectorMapper,
    doubleSha256DigestBEMapper,
    filterTypeMapper
  }
  import profile.api._

  implicit private val bigIntMapper: BaseColumnType[BigInt] =
    appConfig.driver match {
      case SQLite     => mappers.bigIntMapper
      case PostgreSQL => mappers.bigIntPostgresMapper
    }

  class CompactFilterTable(tag: Tag)
      extends Table[CompactFilterDb](tag, schemaName, "cfilters") {

    def hash = column[DoubleSha256DigestBE]("hash")

    def filterType = column[FilterType]("filter_type")

    def bytes = column[ByteVector]("bytes")

    def height = column[Int]("height")

    def blockHash = column[DoubleSha256DigestBE]("block_hash", O.PrimaryKey)

    def heightIndex = index("cfilters_height_index", height)

    def hashIndex = index("cfilters_hash_index", hash)

    override def * = {
      (hash, filterType, bytes, height, blockHash).<>(
        CompactFilterDb.apply,
        CompactFilterDb.unapply
      )
    }
  }

  override val table: profile.api.TableQuery[CompactFilterTable] = {
    TableQuery[CompactFilterTable]
  }

  private lazy val blockHeaderTable
      : profile.api.TableQuery[BlockHeaderDAO#BlockHeaderTable] = {
    BlockHeaderDAO().table
  }

  override def createAll(
      filters: Vector[CompactFilterDb]
  ): Future[Vector[CompactFilterDb]] = {
    createAllNoAutoInc(ts = filters, database = safeDatabase)
  }

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      ids: Vector[DoubleSha256DigestBE]
  ): Query[Table[CompactFilterDb], CompactFilterDb, Seq] = {
    table.filter(_.blockHash.inSet(ids))
  }

  override protected def findAll(
      ts: Vector[CompactFilterDb]
  ): Query[Table[CompactFilterDb], CompactFilterDb, Seq] = {
    findByPrimaryKeys(ts.map(_.blockHashBE))
  }

  def findByBlockHash(
      hash: DoubleSha256DigestBE
  ): Future[Option[CompactFilterDb]] = {
    read(hash)
  }

  def findByBlockHashes(
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[Vector[CompactFilterDb]] = {
    val action = findByPrimaryKeys(hashes).result
    safeDatabase.runVec(action)
  }

  /** Retrieves a [[CompactFilterDb]] at the given height */
  def getAtHeight(height: Int): Future[Vector[CompactFilterDb]] = {
    val query = getAtHeightQuery(height)
    safeDatabase.runVec(query)
  }

  private def getAtHeightQuery(
      height: Int): profile.StreamingProfileAction[Seq[
                                                     CompactFilterDb
                                                   ],
                                                   CompactFilterDb,
                                                   Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Returns the maximum block height from our database */
  def maxHeight: Future[Int] = {
    val query = maxHeightQuery
    val result = safeDatabase.run(query)
    result
  }

  private val maxHeightQuery
      : profile.ProfileAction[Int, NoStream, Effect.Read] = {
    val query = table.map(_.height).max.getOrElse(0).result
    query
  }

  /** Gets filters between (inclusive) from and to, could be out of order */
  def getBetweenHeights(from: Int, to: Int): Future[Vector[CompactFilterDb]] = {
    val query = getBetweenHeightsQuery(from, to)
    safeDatabase.runVec(query)
  }

  private def getBetweenHeightsQuery(
      from: Int,
      to: Int
  ): profile.StreamingProfileAction[Seq[
                                      CompactFilterDb
                                    ],
                                    CompactFilterDb,
                                    Effect.Read] = {
    table
      .filter(header => header.height >= from && header.height <= to)
      .sortBy(_.height)
      .result
  }

  private val bestFilterQuery = {
    val join = table
      .join(blockHeaderTable)
      .on(_.blockHash === _.hash)
      .sortBy(_._1.height.desc)
      // just take the last 2016 headers, if we have a reorg larger than
      // this we will not be able to retrieve that header
      .take(appConfig.chain.difficultyChangeInterval)

    val maxQuery = join.map(_._2.chainWork).max

    join
      .filter(_._2.chainWork === maxQuery)
      .take(1)
      .map(_._1)
      .result
  }

  /** Gets the heaviest filter from the database */
  def getBestFilter: Future[Option[CompactFilterDb]] = {
    safeDatabase.run(bestFilterQuery).map(_.headOption)
  }

  private val bestFilterHeightQuery
      : DBIOAction[Option[Int], NoStream, Effect.Read] = {
    table.map(_.height).max.result
  }

  def getBestFilterHeight: Future[Int] = {
    safeDatabase.run(bestFilterHeightQuery).map(_.getOrElse(0))
  }
}
