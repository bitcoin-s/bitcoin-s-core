package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.core.api.wallet.db.{
  AddressDb,
  AddressRecord,
  ScriptPubKeyDb
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{HDAccount, HDChainType, HDCoinType, HDPurpose}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.lifted.ForeignKeyQuery

import scala.concurrent.{ExecutionContext, Future}

case class AddressDAO()(implicit
    ec: ExecutionContext,
    config: WalletAppConfig
) extends CRUD[AddressRecord, BitcoinAddress]
    with SlickUtil[AddressRecord, BitcoinAddress] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[AddressTable] =
    TableQuery[AddressTable]

  private lazy val spendingInfoTable
      : profile.api.TableQuery[SpendingInfoDAO#SpendingInfoTable] = {
    SpendingInfoDAO().table
  }

  private lazy val spkTable
      : profile.api.TableQuery[ScriptPubKeyDAO#ScriptPubKeyTable] = {
    ScriptPubKeyDAO().table
  }

  override def createAll(
      ts: Vector[AddressRecord]
  ): Future[Vector[AddressRecord]] =
    createAllNoAutoInc(ts, safeDatabase)

  def createAction(addressDb: AddressDb): DBIOAction[
    AddressDb,
    NoStream,
    Effect.Read with Effect.Write with Effect.Transactional
  ] = {
    val spkFind =
      spkTable.filter(_.scriptPubKey === addressDb.scriptPubKey).result
    val actions = for {
      spkOpt <- spkFind.headOption
      _ <- spkOpt match {
        case Some(foundSpk) =>
          table += AddressRecord.fromAddressDb(addressDb, foundSpk.id.get)
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += (ScriptPubKeyDb(
                addressDb.scriptPubKey
              ))
          } yield {
            val record = AddressRecord.fromAddressDb(addressDb, newSpkId)
            table += record
          }).flatten
      }
      addr <- table.filter(_.address === addressDb.address).result.headOption
      spk <-
        spkTable
          .filter(_.scriptPubKey === addressDb.scriptPubKey)
          .result
          .headOption
    } yield (addr, spk)

    actions.map {
      case (Some(addr), Some(spk)) => addr.toAddressDb(spk.scriptPubKey)
      case _ =>
        throw new SQLException(
          s"Unexpected result: Cannot create either a address or a SPK record for $addressDb"
        )
    }
  }

  def create(addressDb: AddressDb): Future[AddressDb] = {
    val actions = createAction(addressDb)
    safeDatabase
      .run(actions)
  }

  def upsert(addressDb: AddressDb): Future[AddressDb] = {
    val spkFind =
      spkTable.filter(_.scriptPubKey === addressDb.scriptPubKey).result
    val actions = for {
      spkOpt: Option[ScriptPubKeyDb] <- spkFind.headOption
      _ <- spkOpt match {
        case Some(foundSpk) =>
          table.insertOrUpdate(
            AddressRecord.fromAddressDb(addressDb, foundSpk.id.get)
          )
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += ScriptPubKeyDb(
                addressDb.scriptPubKey
              )
          } yield table.insertOrUpdate(
            AddressRecord.fromAddressDb(addressDb, newSpkId)
          )).flatten
      }
      addr <- table.filter(_.address === addressDb.address).result.headOption
      spk <-
        spkTable
          .filter(_.scriptPubKey === addressDb.scriptPubKey)
          .result
          .headOption
    } yield (addr, spk)

    safeDatabase
      .run(actions)
      .map {
        case (Some(addr), Some(spk)) => addr.toAddressDb(spk.scriptPubKey)
        case _ =>
          throw new SQLException(
            s"Unexpected result: Cannot upsert either a address or a SPK record for $addressDb"
          )
      }
  }

  def delete(addressDb: AddressDb): Future[Int] = {
    val spkDelete =
      spkTable.filter(_.scriptPubKey === addressDb.scriptPubKey).delete
    val addrDelete = table.filter(_.address === addressDb.address).delete
    safeDatabase
      .run(DBIO.sequence(Seq(addrDelete, spkDelete)))
      .map(_.sum)
  }

  /** Finds the rows that correlate to the given primary keys */
  override def findByPrimaryKeys(
      addresses: Vector[BitcoinAddress]
  ): Query[AddressTable, AddressRecord, Seq] =
    table.filter(_.address.inSet(addresses))

  override def findAll(
      ts: Vector[AddressRecord]
  ): Query[AddressTable, AddressRecord, Seq] =
    findByPrimaryKeys(ts.map(_.address))

  def findAllAddressesAction()
      : DBIOAction[Vector[AddressDb], NoStream, Effect.Read] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    query.result.map { res =>
      res.map { case (addrRec, spkRec) =>
        addrRec.toAddressDb(spkRec.scriptPubKey)
      }.toVector
    }
  }

  def findAllAddresses(): Future[Vector[AddressDb]] = {
    val action = findAllAddressesAction()
    safeDatabase
      .runVec(action)

  }

  def findAddress(addr: BitcoinAddress): Future[Option[AddressDb]] = {
    safeDatabase.run(findAddressAction(addr))
  }

  def findAddressAction(
      addr: BitcoinAddress
  ): DBIOAction[Option[AddressDb], NoStream, Effect.Read] = {
    table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
      .filter(_._1.address === addr)
      .result
      .map(_.headOption)
      .map(res =>
        res.map { case (addrRec, spkRec) =>
          addrRec.toAddressDb(spkRec.scriptPubKey)
        })
  }

  private def addressesForAccountQuery(accountIndex: Int): Query[
    (AddressTable, ScriptPubKeyDAO#ScriptPubKeyTable),
    (AddressRecord, ScriptPubKeyDb),
    Seq
  ] =
    table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
      .filter(_._1.accountIndex === accountIndex)

  def findAllAddressDbForAccount(
      account: HDAccount
  ): Future[Vector[AddressDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
      .filter(_._1.purpose === account.purpose)
      .filter(_._1.accountIndex === account.index)
      .filter(_._1.accountCoin === account.coin.coinType)

    safeDatabase
      .runVec(query.result)
      .map(res =>
        res.map { case (addrRec, spkRec) =>
          addrRec.toAddressDb(spkRec.scriptPubKey)
        })
  }

  def findAllForAccount(account: HDAccount): Future[Vector[AddressRecord]] = {
    val action = findAllForAccountAction(account)
    safeDatabase.runVec(action)
  }

  def findAllForAccountAction(
      account: HDAccount
  ): DBIOAction[Vector[AddressRecord], NoStream, Effect.Read] = {
    val query = table
      .filter(_.purpose === account.purpose)
      .filter(_.accountIndex === account.index)
      .filter(_.accountCoin === account.coin.coinType)
    query.result.map(_.toVector)
  }

  def findMostRecentChangeAction(
      hdAccount: HDAccount
  ): DBIOAction[Option[AddressDb], NoStream, Effect.Read] = {
    val action =
      findMostRecentForChain(hdAccount, HDChainType.Change)
    action.map(_.map { case (addrRec, spkRec) =>
      addrRec.toAddressDb(spkRec.scriptPubKey)
    })
  }

  /** Finds the most recent change address in the wallet, if any
    */
  def findMostRecentChange(hdAccount: HDAccount): Future[Option[AddressDb]] = {
    val action = findMostRecentChangeAction(hdAccount)
    safeDatabase.run(action)
  }

  /** Finds all public keys in the wallet */
  def findAllPubkeys(): Future[Vector[ECPublicKey]] = {
    val query = table.map(_.ecPublicKey).distinct
    safeDatabase.run(query.result).map(_.toVector)
  }

  /** Finds all SPKs in the wallet */
  def findAllSPKs(): Future[Vector[ScriptPubKey]] = {
    val query = table.join(spkTable).on(_.scriptPubKeyId === _.id)
    safeDatabase.run(query.result).map(_.toVector.map(_._2.scriptPubKey))
  }

  def getUnusedAddresses: Future[Vector[AddressDb]] = {
    val query = {
      val joineWithSpks = table.join(spkTable).on(_.scriptPubKeyId === _.id)
      val joinedWithSpendingInfo =
        joineWithSpks
          .joinLeft(spendingInfoTable)
          .on(_._1.scriptPubKeyId === _.scriptPubKeyId)
      joinedWithSpendingInfo.filter(_._2.isEmpty)
    }
    safeDatabase
      .runVec(query.result)
      .map(res =>
        res.map { case ((addrRec, spkRec), _) =>
          addrRec.toAddressDb(spkRec.scriptPubKey)
        })
  }

  def getUnusedAddresses(hdAccount: HDAccount): Future[Vector[AddressDb]] = {
    getUnusedAddresses.map(_.filter(_.path.account == hdAccount))
  }

  def getSpentAddresses(hdAccount: HDAccount): Future[Vector[AddressDb]] = {
    val query =
      table
        .join(spkTable)
        .on(_.scriptPubKeyId === _.id)
        .join(spendingInfoTable)
        .on(_._1.scriptPubKeyId === _.scriptPubKeyId)
        .filter(_._2.state.inSet(TxoState.spentStates))
        .map(_._1)

    safeDatabase
      .runVec(query.result)
      .map(res =>
        res.map { case (addrRec, spkRec) =>
          addrRec.toAddressDb(spkRec.scriptPubKey)
        })
      .map(_.filter(_.path.account == hdAccount))
  }

  def getFundedAddresses(
      account: HDAccount): Future[Vector[(AddressDb, CurrencyUnit)]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
      .join(spendingInfoTable)
      .on(_._1.scriptPubKeyId === _.scriptPubKeyId)
      .filter(_._2.state.inSet(TxoState.receivedStates))

    safeDatabase
      .runVec(query.result)
      .map(_.map { case ((addrRec, spkRec), utxoDb) =>
        (addrRec.toAddressDb(spkRec.scriptPubKey), utxoDb.value)
      })
      .map(_.filter(_._1.path.account == account))
  }

  def findByScriptPubKey(spk: ScriptPubKey): Future[Option[AddressDb]] = {
    findByScriptPubKeys(Vector(spk)).map(_.headOption)
  }

  def findByScriptPubKeys(
      spks: Vector[ScriptPubKey]
  ): Future[Vector[AddressDb]] = {
    safeDatabase.run(findByScriptPubKeysAction(spks))
  }

  def findByScriptPubKeysAction(
      spks: Vector[ScriptPubKey]
  ): DBIOAction[Vector[AddressDb], NoStream, Effect.Read] = {
    table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
      .filter(_._2.scriptPubKey.inSet(spks))
      .result
      .map(_.map { case (addrRec, spkRec) =>
        addrRec.toAddressDb(spkRec.scriptPubKey)
      }.toVector)
  }

  private def findMostRecentForChain(
      account: HDAccount,
      chain: HDChainType
  ): DBIOAction[Option[
                  (AddressRecord, ScriptPubKeyDb)
                ],
                NoStream,
                Effect.Read] = {
    addressesForAccountQuery(account.index)
      .filter(_._1.purpose === account.purpose)
      .filter(_._1.accountCoin === account.coin.coinType)
      .filter(_._1.accountChainType === chain)
      .sortBy(_._1.addressIndex.desc)
      .take(1)
      .result
      .headOption
  }

  def findMostRecentExternalAction(
      hdAccount: HDAccount
  ): DBIOAction[Option[AddressDb], NoStream, Effect.Read] = {
    val action = findMostRecentForChain(hdAccount, HDChainType.External)
    action.map(_.map { case (addrRec, spkRec) =>
      addrRec.toAddressDb(spkRec.scriptPubKey)
    })
  }

  /** Finds the most recent external address in the wallet, if any
    */
  def findMostRecentExternal(
      hdAccount: HDAccount
  ): Future[Option[AddressDb]] = {
    val action = findMostRecentExternalAction(hdAccount)
    safeDatabase.run(action)
  }

  /** todo: this needs design rework. todo:
    * https://github.com/bitcoin-s/bitcoin-s-core/pull/391#discussion_r274188334
    */
  class AddressTable(tag: Tag)
      extends Table[AddressRecord](tag, schemaName, "addresses") {

    def purpose: Rep[HDPurpose] = column("hd_purpose")

    def accountCoin: Rep[HDCoinType] = column("hd_coin")

    def accountIndex: Rep[Int] = column("account_index")

    def accountChainType: Rep[HDChainType] = column("hd_chain_type")

    def addressIndex: Rep[Int] = column("address_index")

    def address: Rep[BitcoinAddress] = column("address", O.PrimaryKey)

    def ecPublicKey: Rep[ECPublicKey] = column("pubkey")

    def hashedPubKey: Rep[Sha256Hash160Digest] = column("hashed_pubkey")

    def scriptPubKeyId: Rep[Long] = column("script_pub_key_id", O.Unique)

    def scriptWitness: Rep[Option[ScriptWitness]] = column("script_witness")

    override def * =
      (
        purpose,
        accountCoin,
        accountIndex,
        accountChainType,
        addressIndex,
        address,
        ecPublicKey,
        hashedPubKey,
        scriptPubKeyId,
        scriptWitness
      ).<>(AddressRecord.apply, AddressRecord.unapply)

    def fk_scriptPubKeyId: ForeignKeyQuery[_, ScriptPubKeyDb] = {
      foreignKey(
        "fk_spk",
        sourceColumns = scriptPubKeyId,
        targetTableQuery = spkTable
      )(_.id)
    }

  }
}
