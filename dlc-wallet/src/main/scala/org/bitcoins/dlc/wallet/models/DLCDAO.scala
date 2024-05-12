package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.protocol.tlv.DLCSerializationVersion
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import scodec.bits.ByteVector
import slick.lifted._

import java.net.InetSocketAddress
import java.sql.SQLException
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class DLCDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[DLCDb, Sha256Digest]
    with SlickUtil[DLCDb, Sha256Digest]
    with DLCIdDaoUtil[DLCDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCTable] = TableQuery[DLCTable]

  private lazy val contactTable
      : slick.lifted.TableQuery[DLCContactDAO#DLCContactTable] =
    DLCContactDAO().table

  override def createAll(ts: Vector[DLCDb]): Future[Vector[DLCDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256Digest]
  ): Query[DLCTable, DLCDb, Seq] =
    table.filter(_.dlcId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256Digest
  ): Query[DLCTable, DLCDb, Seq] = {
    table
      .filter(_.dlcId === id)
  }

  override def findAll(dlcs: Vector[DLCDb]): Query[DLCTable, DLCDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.dlcId))

  override def findByDLCIdsAction(
      dlcIds: Vector[Sha256Digest]): DBIOAction[Vector[
                                                  DLCDb
                                                ],
                                                profile.api.NoStream,
                                                profile.api.Effect.Read] = {
    val q = table.filter(_.dlcId.inSet(dlcIds))
    q.result.map(_.toVector)
  }

  override def deleteByDLCIdAction(
      dlcId: Sha256Digest
  ): DBIOAction[Int, profile.api.NoStream, profile.api.Effect.Write] = {
    val q = table.filter(_.dlcId === dlcId)
    q.delete
  }

  def findByDLCIds(dlcIds: Vector[Sha256Digest]): Future[Vector[DLCDb]] = {
    val action = table.filter(_.dlcId.inSet(dlcIds)).result
    safeDatabase.runVec(action)
  }

  def findByTempContractId(
      tempContractId: Sha256Digest
  ): Future[Option[DLCDb]] = {
    val q = table.filter(_.tempContractId === tempContractId)

    safeDatabase.run(q.result).map(_.headOption)
  }

  def findByTempContractId(
      tempContractId: Sha256DigestBE
  ): Future[Option[DLCDb]] =
    findByTempContractId(tempContractId.flip)

  def findByContractId(contractId: ByteVector): Future[Option[DLCDb]] = {
    val q = table.filter(_.contractId === contractId)

    safeDatabase.run(q.result).map(_.headOption)
  }

  def findByFundingOutPoint(
      outPoint: TransactionOutPoint
  ): Future[Option[DLCDb]] = {
    val q = table.filter(_.fundingOutPointOpt === outPoint)

    safeDatabase.run(q.result).map(_.headOption)
  }

  def findByFundingOutPoints(
      outPoints: Vector[TransactionOutPoint]
  ): Future[Vector[DLCDb]] = {
    val q = table.filter(_.fundingOutPointOpt.inSet(outPoints))

    safeDatabase.runVec(q.result)
  }

  def findByFundingTxId(txId: DoubleSha256DigestBE): Future[Vector[DLCDb]] = {
    val q = table.filter(_.fundingTxIdOpt === txId)

    safeDatabase.runVec(q.result)
  }

  def findByContactId(contactId: String): Future[Vector[DLCDb]] = {
    safeDatabase.run(findByContactIdAction(contactId))
  }

  def findByContactIdAction(
      contactId: String
  ): DBIOAction[Vector[DLCDb], NoStream, Effect.Read] = {
    val peer: Option[String] = Some(contactId)
    table.filter(_.peerOpt === peer).result.map(_.toVector)
  }

  def findByStateAction(
      state: DLCState
  ): DBIOAction[Vector[DLCDb], NoStream, Effect.Read] = {
    table.filter(_.state === state).result.map(_.toVector)
  }

  def findByStatesAction(
      states: Vector[DLCState]
  ): DBIOAction[Vector[DLCDb], NoStream, Effect.Read] = {
    table.filter(_.state.inSet(states)).result.map(_.toVector)
  }

  def findByState(state: DLCState): Future[Vector[DLCDb]] = {
    safeDatabase.run(findByStateAction(state))
  }

  def updateDLCContactMapping(
      dlcId: Sha256Digest,
      contcatId: InetSocketAddress
  ): Future[Unit] = {
    val contactQuery = contactTable.filter(_.address === contcatId)

    val action = for {
      contactExists <- contactQuery.exists.result
      _ <-
        if (contactExists) DBIO.successful(())
        else DBIO.failed(new SQLException(s"Unknown contact: $contcatId"))
      res <- updatePeerAction(
        dlcId,
        Some(contcatId.getHostName + ":" + contcatId.getPort)
      )
    } yield res

    safeDatabase.run(action).map(_ => ())
  }

  def deleteDLCContactMapping(dlcId: Sha256Digest): Future[Unit] = {
    val action = updatePeerAction(dlcId, None)

    safeDatabase.run(action).map(_ => ())
  }

  private def updatePeerAction(
      dlcId: Sha256Digest,
      peerOpt: Option[String]
  ): DBIOAction[Int, NoStream, Effect.Read with Effect.Write] = {
    val dlcQuery = table.filter(_.dlcId === dlcId)

    for {
      dlcOpt <- dlcQuery.result.headOption
      res <- dlcOpt match {
        case None => DBIO.failed(new SQLException(s"Unknown DLC: $dlcId"))
        case Some(dlc) =>
          dlcQuery.update(dlc.copy(peerOpt = peerOpt))
      }
    } yield res
  }

  def findByDLCSerializationVersion(
      version: DLCSerializationVersion
  ): Future[Vector[DLCDb]] = {
    val action = table.filter(_.serializationVersion === version).result
    safeDatabase
      .run(action)
      .map(_.toVector)
  }

  class DLCTable(tag: Tag)
      extends Table[DLCDb](tag, schemaName, "global_dlc_data") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id", O.PrimaryKey)

    def tempContractId: Rep[Sha256Digest] =
      column("temp_contract_id", O.Unique)

    def contractId: Rep[Option[ByteVector]] =
      column("contract_id", O.Unique)

    def protocolVersion: Rep[Int] = column("protocol_version")

    def state: Rep[DLCState] = column("state")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def account: Rep[HDAccount] = column("account")

    def changeIndex: Rep[HDChainType] = column("change_index")

    def keyIndex: Rep[Int] = column("key_index")

    def feeRate: Rep[SatoshisPerVirtualByte] = column("fee_rate")

    def fundOutputSerialId: Rep[UInt64] = column("fund_output_serial_id")

    def lastUpdated: Rep[Instant] = column("last_updated")

    def fundingOutPointOpt: Rep[Option[TransactionOutPoint]] =
      column("funding_outpoint")

    def fundingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("funding_tx_id")

    def closingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("closing_tx_id")

    def aggregateSignatureOpt: Rep[Option[SchnorrDigitalSignature]] = column(
      "aggregate_signature"
    )

    def serializationVersion: Rep[DLCSerializationVersion] = column(
      "serialization_version"
    )

    def peerOpt: Rep[Option[String]] = column("peer")

    override def * : ProvenShape[DLCDb] =
      (
        dlcId,
        tempContractId,
        contractId,
        protocolVersion,
        state,
        isInitiator,
        account,
        changeIndex,
        keyIndex,
        feeRate,
        fundOutputSerialId,
        lastUpdated,
        fundingOutPointOpt,
        fundingTxIdOpt,
        closingTxIdOpt,
        aggregateSignatureOpt,
        serializationVersion,
        peerOpt
      ).<>(DLCDb.apply, DLCDb.unapply)
  }
}
