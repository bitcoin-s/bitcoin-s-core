package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv.NegotiationFieldsTLV
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCAcceptDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig
) extends CRUD[DLCAcceptDb, Sha256Digest]
    with SlickUtil[DLCAcceptDb, Sha256Digest]
    with DLCIdDaoUtil[DLCAcceptDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCAcceptTable] = TableQuery[DLCAcceptTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(ts: Vector[DLCAcceptDb]): Future[Vector[DLCAcceptDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override def findByPrimaryKeys(
      ids: Vector[Sha256Digest]
  ): Query[DLCAcceptTable, DLCAcceptDb, Seq] =
    table.filter(_.dlcId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256Digest
  ): Query[DLCAcceptTable, DLCAcceptDb, Seq] = {
    table
      .filter(_.dlcId === id)
  }

  override def findAll(
      dlcs: Vector[DLCAcceptDb]
  ): Query[DLCAcceptTable, DLCAcceptDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.dlcId))

  override def findByDLCIdsAction(
      dlcIds: Vector[Sha256Digest]
  ): DBIOAction[Vector[
                  DLCAcceptDb
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

  class DLCAcceptTable(tag: Tag)
      extends Table[DLCAcceptDb](tag, schemaName, "accept_dlc_data") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id", O.PrimaryKey)

    def fundingPubKey: Rep[ECPublicKey] = column("funding_pub_key")

    def payoutAddress: Rep[BitcoinAddress] = column("payout_address")

    def payoutSerialId: Rep[UInt64] = column("payout_serial_id")

    def collateral: Rep[CurrencyUnit] = column("collateral")

    def changeAddress: Rep[BitcoinAddress] = column("change_address")

    def changeSerialId: Rep[UInt64] = column("change_serial_id")

    def negotiationFields: Rep[NegotiationFieldsTLV] = column(
      "negotiation_fields"
    )

    def * : ProvenShape[DLCAcceptDb] =
      (
        dlcId,
        fundingPubKey,
        payoutAddress,
        payoutSerialId,
        collateral,
        changeAddress,
        changeSerialId,
        negotiationFields
      ).<>(DLCAcceptDb.apply, DLCAcceptDb.unapply)

    def fk: ForeignKeyQuery[?, DLCDb] =
      foreignKey(
        "fk_dlc_id",
        sourceColumns = dlcId,
        targetTableQuery = dlcTable
      )(_.dlcId)
  }
}
