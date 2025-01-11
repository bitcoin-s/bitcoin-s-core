package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, CompactFilterHeaderDb}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.db.{CRUD, SlickUtil}

import scala.concurrent.{ExecutionContext, Future}

case class CompactFilterHeaderDAO()(implicit
    ec: ExecutionContext,
    override val appConfig: ChainAppConfig
) extends CRUD[CompactFilterHeaderDb, DoubleSha256DigestBE]
    with SlickUtil[CompactFilterHeaderDb, DoubleSha256DigestBE] {
  import profile.api._
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers.doubleSha256DigestBEMapper

  implicit private val bigIntMapper: BaseColumnType[BigInt] =
    appConfig.driver match {
      case SQLite     => mappers.bigIntMapper
      case PostgreSQL => mappers.bigIntPostgresMapper
    }

  class CompactFilterHeaderTable(tag: Tag)
      extends Table[CompactFilterHeaderDb](tag, schemaName, "cfheaders") {

    def hash = column[DoubleSha256DigestBE]("hash", O.PrimaryKey)

    def filterHash = column[DoubleSha256DigestBE]("filter_hash")

    def previousFilterHeader =
      column[DoubleSha256DigestBE]("previous_filter_header")

    def blockHash = column[DoubleSha256DigestBE]("block_hash")

    def height = column[Int]("height")

    def heightIndex = index("cfheaders_height_index", height)

    def blockHashIndex = index("cfheaders_block_hash_index", blockHash)

    override def * = {
      (hash, filterHash, previousFilterHeader, blockHash, height).<>(
        CompactFilterHeaderDb.apply,
        CompactFilterHeaderDb.unapply
      )
    }
  }

  override val table: profile.api.TableQuery[CompactFilterHeaderTable] = {
    TableQuery[CompactFilterHeaderTable]
  }

  private lazy val blockHeaderTable
      : profile.api.TableQuery[BlockHeaderDAO#BlockHeaderTable] = {
    BlockHeaderDAO().table
  }

  override def createAll(
      filterHeaders: Vector[CompactFilterHeaderDb]
  ): Future[Vector[CompactFilterHeaderDb]] = {
    createAllNoAutoInc(ts = filterHeaders, database = safeDatabase)
  }

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      ids: Vector[DoubleSha256DigestBE]
  ): Query[Table[CompactFilterHeaderDb], CompactFilterHeaderDb, Seq] =
    table.filter(_.hash.inSet(ids))

  override protected def findAll(
      ts: Vector[CompactFilterHeaderDb]
  ): Query[Table[CompactFilterHeaderDb], CompactFilterHeaderDb, Seq] =
    findByPrimaryKeys(ts.map(_.hashBE))

  def findByHash(
      hash: DoubleSha256DigestBE
  ): Future[Option[CompactFilterHeaderDb]] = {
    read(hash)
  }

  def findByHashes(
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[Vector[CompactFilterHeaderDb]] = {
    val query = findByPrimaryKeys(hashes).result
    safeDatabase.runVec(query)
  }

  def findByBlockHash(
      hash: DoubleSha256DigestBE
  ): Future[Option[CompactFilterHeaderDb]] = {
    val query = table.filter(_.blockHash === hash).take(1)
    safeDatabase.runVec(query.result).map(_.headOption)
  }

  def findAllByBlockHashes(
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[Vector[CompactFilterHeaderDb]] = {
    val query = table
      .filter(_.blockHash.inSet(hashes))
      .sortBy(_.height)
    safeDatabase.runVec(query.result)
  }

  /** Retrieves a [[CompactFilterHeaderDb]] at the given height */
  def getAtHeight(height: Int): Future[Vector[CompactFilterHeaderDb]] = {
    val query = getAtHeightQuery(height)
    safeDatabase.runVec(query)
  }

  private def getAtHeightQuery(
      height: Int
  ): slick.sql.FixedSqlStreamingAction[Seq[
                                         CompactFilterHeaderDb
                                       ],
                                       CompactFilterHeaderDb,
                                       Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Returns the maximum block height from our database */
  def maxHeight: Future[Int] = {
    val query = maxHeightQuery
    val result = database.run(query)
    result
  }

  private val maxHeightQuery
      : profile.ProfileAction[Int, NoStream, Effect.Read] = {
    val query = table.map(_.height).max.getOrElse(0).result
    query
  }

  private val bestFilterHeaderQuery = {
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

  /** Fetches the best filter header from the database _without_ context that
    * it's actually in our best blockchain. For instance, this filter header
    * could be reorged out for whatever reason.
    * @see
    *   https://github.com/bitcoin-s/bitcoin-s/issues/1919#issuecomment-682041737
    */
  def getBestFilterHeader: Future[Option[CompactFilterHeaderDb]] = {
    safeDatabase.run(bestFilterHeaderQuery).map(_.headOption)
  }

  private val bestFilterHeaderHeightQuery: Rep[Option[Int]] = {
    table.map(_.height).max
  }

  def getBestFilterHeaderHeight: Future[Int] = {
    safeDatabase.run(bestFilterHeaderHeightQuery.result).map {
      filterHeaderHeightOpt =>
        filterHeaderHeightOpt.getOrElse(0)
    }
  }

  /** This looks for best filter headers whose
    * [[CompactFilterHeaderDb.blockHashBE]] are associated with the given
    * [[BlockHeaderDb.hashBE]] given as a parameter.
    */
  def getBestFilterHeaderForHeaders(
      headers: Vector[BlockHeaderDb]
  ): Future[Option[CompactFilterHeaderDb]] = {
    val hashes = headers.map(_.hashBE)
    val join = table
      .join(blockHeaderTable)
      .on(_.blockHash === _.hash)

    val joinedWithHashes = join
      .filter { case (filterTable, _) =>
        filterTable.blockHash.inSet(hashes)
      }

    val maxQuery = joinedWithHashes.map(_._2.chainWork).max

    val query =
      joinedWithHashes.filter(_._2.chainWork === maxQuery).take(1).map(_._1)

    for {
      filterOpt <-
        safeDatabase
          .run(query.result)
          .map(_.headOption)
    } yield filterOpt
  }

  def getBetweenHeights(
      from: Int,
      to: Int
  ): Future[Vector[CompactFilterHeaderDb]] = {
    val query = getBetweenHeightsQuery(from, to)
    safeDatabase.runVec(query)
  }

  def getBetweenHeightsQuery(
      from: Int,
      to: Int
  ): profile.StreamingProfileAction[Seq[
                                      CompactFilterHeaderDb
                                    ],
                                    CompactFilterHeaderDb,
                                    Effect.Read] = {
    table.filter(header => header.height >= from && header.height <= to).result
  }

}
