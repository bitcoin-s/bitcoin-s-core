package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.*
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config.*
import slick.ast.{TableExpansion, TableNode}

import java.sql.{PreparedStatement, SQLException}
import scala.concurrent.{ExecutionContext, Future}

case class SpendingInfoDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: WalletAppConfig
) extends CRUDAutoInc[UTXORecord] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import org.bitcoins.core.currency.currencyUnitNumeric

  /** The table inside our database we are inserting into */
  override val table: profile.api.TableQuery[SpendingInfoTable] =
    profile.api.TableQuery[SpendingInfoTable]

  private lazy val addrTable
      : profile.api.TableQuery[AddressDAO#AddressTable] = {
    AddressDAO()(ec, appConfig).table
  }

  private lazy val txTable: profile.api.TableQuery[
    IncomingTransactionDAO#IncomingTransactionTable
  ] = {
    IncomingTransactionDAO().table
  }

  private lazy val tagTable
      : profile.api.TableQuery[AddressTagDAO#AddressTagTable] = {
    AddressTagDAO().table
  }

  private lazy val spkTable
      : profile.api.TableQuery[ScriptPubKeyDAO#ScriptPubKeyTable] = {
    ScriptPubKeyDAO().table
  }

  def create(si: SpendingInfoDb): Future[SpendingInfoDb] = {
    val actions = for {
      utxo: UTXORecord <- insertAction(si)
      spk <-
        spkTable
          .filter(_.id === utxo.scriptPubKeyId)
          .result
          .headOption
    } yield (utxo, spk)

    safeDatabase
      .run(actions)
      .map {
        case (utxo, Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
        case _ =>
          throw new SQLException(
            s"Unexpected result: Cannot create either a UTXO or a SPK record for $si"
          )
      }
  }

  def createUnlessAction(
      si: SpendingInfoDb
  )(condition: (UTXORecord, UTXORecord) => Boolean)
      : DBIOAction[SpendingInfoDb, NoStream, Effect.Write & Effect.Read] = {
    val actions: DBIOAction[(UTXORecord, Option[ScriptPubKeyDb]),
                            NoStream,
                            Effect.Read & Effect.Write] = for {
      foundOpt <- table.filter(_.outPoint === si.outPoint).result.headOption
      cond <- foundOpt match {
        case Some(foundUtxo) =>
          val utxoToCreate =
            UTXORecord.fromSpendingInfoDb(si, foundUtxo.scriptPubKeyId)
          slick.dbio.DBIOAction.successful(condition(foundUtxo, utxoToCreate))
        case None => slick.dbio.DBIOAction.successful(false)
      }
      utxo <-
        if (cond) slick.dbio.DBIOAction.successful(foundOpt.get)
        else insertAction(si)
      spk <-
        spkTable
          .filter(_.id === utxo.scriptPubKeyId)
          .result
          .headOption
    } yield (utxo, spk)

    actions
      .map {
        case (utxo, Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
        case (utxo, None) =>
          throw new RuntimeException(
            s"Unexpected result: Cannot create either a UTXO or a SPK record for spendingInfoDb=$si utxo=$utxo")

      }
  }

  def createUnless(
      si: SpendingInfoDb
  )(condition: (UTXORecord, UTXORecord) => Boolean): Future[SpendingInfoDb] = {
    val action = createUnlessAction(si)(condition)
    safeDatabase.run(action)
  }

  private def insertAction(
      si: SpendingInfoDb
  ): DBIOAction[UTXORecord, NoStream, Effect.Read & Effect.Write] = {
    val query =
      table.returning(table.map(_.id)).into((t, id) => t.copyWithId(id = id))
    for {
      spkOpt <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
      utxo: UTXORecord <- spkOpt match {
        case Some(foundSpk) =>
          val utxo = UTXORecord.fromSpendingInfoDb(si, foundSpk.id.get)
          query += utxo
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += (ScriptPubKeyDb(
                si.output.scriptPubKey
              ))
          } yield {
            val utxo = UTXORecord.fromSpendingInfoDb(si, newSpkId)
            query += utxo
          }).flatten
      }
    } yield utxo
  }

  def upsertAllSpendingInfoDb(
      ts: Vector[SpendingInfoDb]
  ): Future[Vector[SpendingInfoDb]] = {
    FutureUtil.foldLeftAsync(Vector.empty[SpendingInfoDb], ts)((acc, si) =>
      upsert(si).map(res => acc :+ res))
  }

  def upsertAllSpendingInfoDbAction(
      ts: Vector[SpendingInfoDb]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Read & Effect.Write] = {
    slick.dbio.DBIOAction.sequence(ts.map(upsertAction))
  }

  def updateAllSpendingInfoDbAction(
      ts: Vector[SpendingInfoDb]
  ): DBIOAction[Vector[SpendingInfoDb],
                NoStream,
                Effect.Read & Effect.Write] = {
    slick.dbio.DBIOAction.sequence(ts.map(updateAction))
  }

  def updateAllSpendingInfoDb(
      ts: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    val action = updateAllSpendingInfoDbAction(ts)
    safeDatabase.run(action)
  }

  def updateAction(si: SpendingInfoDb)
      : DBIOAction[SpendingInfoDb, NoStream, Effect.Read & Effect.Write] = {
    val actions = for {
      spkOpt <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
      _ <- spkOpt match {
        case Some(foundSpk) =>
          val utxo = UTXORecord.fromSpendingInfoDb(si, foundSpk.id.get)
          table.filter(_.id === utxo.id).update(utxo)
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += ScriptPubKeyDb(
                si.output.scriptPubKey
              )
          } yield {
            val utxo = UTXORecord.fromSpendingInfoDb(si, newSpkId)
            table.filter(_.id === utxo.id).update(utxo)
          }).flatten
      }
      utxo <- table.filter(_.id === si.id.get).result.headOption
      spk <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
    } yield (utxo, spk)
    actions.map {
      case (Some(utxo), Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
      case _ =>
        throw new SQLException(
          s"Unexpected result: Cannot update either a UTXO or a SPK record for $si"
        )
    }
  }
  def update(si: SpendingInfoDb): Future[SpendingInfoDb] = {
    val action = updateAction(si)
    safeDatabase.run(action)
  }

  def upsertAction(si: SpendingInfoDb)
      : DBIOAction[SpendingInfoDb, NoStream, Effect.Read & Effect.Write] = {
    val actions = for {
      spkOpt <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
      _ <- spkOpt match {
        case Some(foundSpk) =>
          table.insertOrUpdate(
            UTXORecord.fromSpendingInfoDb(si, foundSpk.id.get)
          )
        case None =>
          (for {
            newSpkId <-
              (spkTable returning spkTable.map(_.id)) += ScriptPubKeyDb(
                si.output.scriptPubKey
              )
          } yield table.insertOrUpdate(
            UTXORecord.fromSpendingInfoDb(si, newSpkId)
          )).flatten
      }
      utxo <- table.filter(_.outPoint === si.outPoint).result.headOption
      spk <-
        spkTable
          .filter(_.scriptPubKey === si.output.scriptPubKey)
          .result
          .headOption
    } yield (utxo, spk)

    actions.map {
      case (Some(utxo), Some(spk)) => utxo.toSpendingInfoDb(spk.scriptPubKey)
      case _ =>
        throw new SQLException(
          s"Unexpected result: Cannot upsert either a UTXO or a SPK record for $si"
        )
    }
  }

  def upsert(si: SpendingInfoDb): Future[SpendingInfoDb] = {
    val actions = upsertAction(si)
    safeDatabase
      .run(actions)
  }

  def delete(si: SpendingInfoDb): Future[Int] = {
    val action = deleteAction(si)
    safeDatabase.run(action)
  }

  def deleteAction(
      si: SpendingInfoDb
  ): DBIOAction[Int, NoStream, Effect.Write] = {
    deleteSpendingInfoDbAllAction(Vector(si))
  }

  def deleteSpendingInfoDbAllAction(
      sis: Vector[SpendingInfoDb]
  ): DBIOAction[Int, NoStream, Effect.Write] = {
    val ids = sis.map(_.id).flatten
    val query = table.filter(t => t.id.inSet(ids))
    query.delete
  }

  def findAllSpendingInfosAction()
      : DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    for {
      all <- findAllAction()
      utxos <- utxoToInfoAction(all)
    } yield utxos
  }

  def findAllSpendingInfos(): Future[Vector[SpendingInfoDb]] =
    for {
      all <- findAll()
      utxos <- utxoToInfo(all)
    } yield utxos

  /** Fetches all the received TXOs in our DB that are in the given TX
    */
  def findTx(tx: Transaction): Future[Vector[SpendingInfoDb]] =
    findTxs(Vector(tx))

  /** Fetches all received txos in our db that are in the given txs */
  def findTxs(txs: Vector[Transaction]): Future[Vector[SpendingInfoDb]] = {
    findOutputsReceived(txs.map(_.txIdBE))
  }

  /** Fetches all received txos in our db that are in the given txs */
  def findTxsAction(
      txs: Vector[Transaction]
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    findOutputsReceivedAction(txs.map(_.txIdBE))
  }

  def findTxAction(
      tx: Transaction
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    findTxsAction(Vector(tx))
  }

  private def _findOutputsBeingSpentQuery(
      txs: Vector[Transaction]
  ): Vector[Query[SpendingInfoTable, UTXORecord, Seq]] = {
    val outPoints: Vector[TransactionOutPoint] = txs
      .flatMap(_.inputs)
      .map(_.previousOutput)

    val outpointsGrouped = outPoints.grouped(1000)
    val queries = outpointsGrouped.map { outPoints =>
      table.filter { case txo =>
        txo.outPoint.inSet(outPoints)
      }
    }
    queries.toVector
  }

  /** Finds all the outputs being spent in the given transaction
    */
  def findOutputsBeingSpent(tx: Transaction): Future[Vector[SpendingInfoDb]] = {
    findOutputsBeingSpent(Vector(tx))
  }

  private def findOutputsBeingSpentQuery(
      txs: Vector[Transaction]
  ): Vector[Query[
    (SpendingInfoTable, ScriptPubKeyDAO#ScriptPubKeyTable),
    (UTXORecord, ScriptPubKeyDAO#ScriptPubKeyTable#TableElementType),
    Seq
  ]] = {
    _findOutputsBeingSpentQuery(txs).map { query =>
      query.join(spkTable).on(_.scriptPubKeyId === _.id)
    }
  }

  def findOutputsBeingSpent(
      txs: Vector[Transaction]
  ): Future[Vector[SpendingInfoDb]] = {
    val action = findOutputsBeingSpentAction(txs)
    safeDatabase.run(action)
  }

  def findOutputsBeingSpentAction(
      txs: Vector[Transaction]
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    val queries = findOutputsBeingSpentQuery(txs)
    val actions: Vector[
      DBIOAction[Vector[(UTXORecord, ScriptPubKeyDb)], NoStream, Effect.Read]
    ] = {
      queries.map(_.result.map(_.toVector))
    }
    val action: DBIOAction[Vector[
                             (UTXORecord, ScriptPubKeyDb)
                           ],
                           NoStream,
                           Effect.Read] =
      DBIO.sequence(actions).map(_.flatten.toVector)

    action.map(_.map { case (utxo, spk) =>
      utxo.toSpendingInfoDb(spk.scriptPubKey)
    })
  }

  /** Given a TXID, fetches all incoming TXOs and the address the TXO pays to
    */
  def withAddress(
      txid: DoubleSha256DigestBE
  ): Future[Vector[(SpendingInfoDb, AddressDb)]] = {
    def _withAddress: Future[Vector[(UTXORecord, AddressRecord)]] = {
      val query = {
        val filtered = table.filter(_.txid === txid)
        filtered.join(addrTable).on(_.scriptPubKeyId === _.scriptPubKeyId)
      }

      safeDatabase.runVec(query.result)
    }

    for {
      res <- _withAddress
      utxoSpks <- findScriptPubKeysByUtxos(res.map(_._1))
      addrSpks <- findScriptPubKeys(res.map(_._2.scriptPubKeyId))
    } yield {
      res.map(r =>
        (
          r._1.toSpendingInfoDb(utxoSpks(r._1.scriptPubKeyId).scriptPubKey),
          r._2.toAddressDb(addrSpks(r._2.scriptPubKeyId).scriptPubKey)
        ))
    }

  }

  /** Fetches all the incoming TXOs in our DB that are in the transaction with
    * the given TXID
    */
  def findDbsForTx(txid: DoubleSha256DigestBE): Future[Vector[UTXORecord]] = {
    val query = table.filter(_.txid === txid)
    safeDatabase.runVec(query.result)
  }

  /** Joins the spk table on the spending info table with the spk id */
  private val spkJoinQuery = table
    .join(spkTable)
    .on(_.scriptPubKeyId === _.id)

  /** Fetches all the incoming TXOs in our DB that are in the transaction with
    * the given TXID
    */
  def findOutputsReceivedAction(
      txids: Vector[DoubleSha256DigestBE]
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    spkJoinQuery
      .filter(_._1.state.inSet(TxoState.receivedStates))
      .filter(_._1.txid.inSet(txids))
      .result
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
      .map(_.toVector)
  }

  /** Fetches all the incoming TXOs in our DB that are in the transaction with
    * the given TXID
    */
  def findOutputsReceived(
      txids: Vector[DoubleSha256DigestBE]
  ): Future[Vector[SpendingInfoDb]] = {
    safeDatabase.run(findOutputsReceivedAction(txids))
  }

  def findByScriptPubKey(
      scriptPubKey: ScriptPubKey
  ): Future[Vector[SpendingInfoDb]] = {
    val filtered = spkJoinQuery.filter(_._2.scriptPubKey === scriptPubKey)
    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  def findByScriptPubKeyId(scriptPubKeyId: Long): Future[Vector[UTXORecord]] = {
    val filtered = table.filter(_.scriptPubKeyId === scriptPubKeyId)
    safeDatabase.runVec(filtered.result)
  }

  /** Enumerates all unspent TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or
    * [[TxoState.ConfirmedReceived]]
    */
  def _findAllUnspent(): Future[Vector[UTXORecord]] = {
    safeDatabase.run(_findAllUnspentAction())
  }

  /** Enumerates all unspent TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or
    * [[TxoState.ConfirmedReceived]]
    */
  def _findAllUnspentAction()
      : DBIOAction[Vector[UTXORecord], NoStream, Effect.Read] =
    _findByStateAction(TxoState.receivedStates.toVector)

  def _findByStateAction(
      states: Vector[TxoState]
  ): DBIOAction[Vector[UTXORecord], NoStream, Effect.Read] = {
    table.filter(_.state.inSet(states)).result.map(_.toVector)
  }

  def utxoToInfoAction(
      utxos: Vector[UTXORecord]
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    for {
      spks <- findScriptPubKeysAction(utxos)
    } yield utxos.map(utxo =>
      utxo.toSpendingInfoDb(spks(utxo.scriptPubKeyId).scriptPubKey))
  }

  def utxoToInfo(utxos: Vector[UTXORecord]): Future[Vector[SpendingInfoDb]] =
    for {
      spks <- findScriptPubKeysByUtxos(utxos)
    } yield utxos.map(utxo =>
      utxo.toSpendingInfoDb(spks(utxo.scriptPubKeyId).scriptPubKey))

  def infoToUtxo(infos: Vector[SpendingInfoDb]): Future[Vector[UTXORecord]] =
    for {
      spks <- findPublicKeyScriptsBySpendingInfoDb(infos)
    } yield infos.map(utxo =>
      UTXORecord.fromSpendingInfoDb(utxo, spks(utxo.output.scriptPubKey)))

  def findAllUnspent(): Future[Vector[SpendingInfoDb]] = {
    safeDatabase.run(findAllUnspentAction())
  }

  def findAllUnspentAction()
      : DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    for {
      utxos <- _findAllUnspentAction()
      infos <- utxoToInfoAction(utxos)
    } yield infos
  }

  def getBalanceAction(
      accountOpt: Option[HDAccount] = None
  ): DBIOAction[CurrencyUnit, NoStream, Effect.Read] = {
    val account = accountOpt.getOrElse(appConfig.defaultAccount)
    for {
      utxos <- _findAllUnspentAction()
      infos <- utxoToInfoAction(utxos)
      forAccount = filterUtxosByAccount(infos, account)
    } yield forAccount.map(_.output.value).sum
  }

  def getConfirmedBalanceAction(
      accountOpt: Option[HDAccount] = None
  ): DBIOAction[CurrencyUnit, NoStream, Effect.Read] = {
    val account = accountOpt.getOrElse(appConfig.defaultAccount)
    for {
      utxos <- _findByStateAction(Vector(TxoState.ConfirmedReceived))
      infos <- utxoToInfoAction(utxos)
      forAccount = filterUtxosByAccount(infos, account)
    } yield forAccount.map(_.output.value).sum
  }

  def getUnconfirmedBalanceAction(
      accountOpt: Option[HDAccount] = None
  ): DBIOAction[CurrencyUnit, NoStream, Effect.Read] = {
    val account = accountOpt.getOrElse(appConfig.defaultAccount)
    for {
      utxos <- _findByStateAction(TxoState.pendingReceivedStates.toVector)
      infos <- utxoToInfoAction(utxos)
      forAccount = filterUtxosByAccount(infos, account)
    } yield forAccount.map(_.output.value).sum
  }

  private def filterUtxosByAccount(
      utxos: Vector[SpendingInfoDb],
      hdAccount: HDAccount
  ): Vector[SpendingInfoDb] = {
    utxos.filter(utxo =>
      HDAccount.isSameAccount(bip32Path = utxo.privKeyPath,
                              account = hdAccount))
  }

  /** Finds all utxos for a given account */
  def findAllUnspentForAccount(
      hdAccount: HDAccount
  ): Future[Vector[SpendingInfoDb]] = {
    val allUtxosF = findAllUnspent()
    allUtxosF.map(filterUtxosByAccount(_, hdAccount))
  }

  def findAllUnspentForAccountAction(
      hdAccount: HDAccount
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    val allUtxosA = findAllUnspentAction()
    allUtxosA.map(filterUtxosByAccount(_, hdAccount))
  }

  def findAllForAccountAction(
      hdAccount: HDAccount
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    findAllSpendingInfosAction().map(filterUtxosByAccount(_, hdAccount))
  }

  def findAllForAccount(
      hdAccount: HDAccount
  ): Future[Vector[SpendingInfoDb]] = {
    val action = findAllForAccountAction(hdAccount)
    safeDatabase.run(action)
  }

  def findByTxoState(state: TxoState): Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.state === state)

    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  /** Enumerates all TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or
    * [[TxoState.PendingConfirmationsSpent]]
    */
  def findAllPendingConfirmation: Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.state.inSet(TxoState.pendingConfStates))

    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  /** Enumerates all TX outputs in the wallet with the state
    * [[TxoState.BroadcastSpent]] or [[TxoState.BroadcastReceived]]
    */
  def findAllInMempool: Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.state.inSet(TxoState.broadcastStates))

    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  /** Enumerates all TX outpoints in the wallet */
  def findAllOutpoints(): Future[Vector[TransactionOutPoint]] = {
    val query = table.map(_.outPoint)
    safeDatabase.runVec(query.result).map(_.toVector)
  }

  def findByOutPoint(
      outPoint: TransactionOutPoint
  ): Future[Option[SpendingInfoDb]] = {
    findByOutPoints(Vector(outPoint)).map(_.headOption)
  }

  /** Enumerates all TX outpoints in the wallet */
  def findByOutPoints(
      outPoints: Vector[TransactionOutPoint]
  ): Future[Vector[SpendingInfoDb]] = {
    val query = table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
    val filtered = query.filter(_._1.outPoint.inSet(outPoints))
    safeDatabase
      .runVec(filtered.result)
      .map(res =>
        res.map { case (utxoRec, spkRec) =>
          utxoRec.toSpendingInfoDb(spkRec.scriptPubKey)
        })
  }

  def findAllUnspentForTagAction(
      tag: AddressTag
  ): DBIOAction[Vector[SpendingInfoDb], NoStream, Effect.Read] = {
    table
      .join(spkTable)
      .on(_.scriptPubKeyId === _.id)
      .filter(_._1.state.inSet(TxoState.receivedStates))
      .join(addrTable)
      .on(_._1.scriptPubKeyId === _.scriptPubKeyId)
      .join(tagTable)
      .on(_._2.address === _.address)
      .filter(_._2.tagName === tag.tagName)
      .filter(_._2.tagType === tag.tagType)
      .result
      .map(_.toVector)
      .map(_.map { case (((utxoRecord, spkDb), _), _) =>
        utxoRecord.toSpendingInfoDb(spkDb.scriptPubKey)
      })
  }

  def findAllUnspentForTag(tag: AddressTag): Future[Vector[SpendingInfoDb]] = {
    safeDatabase.run(findAllUnspentForTagAction(tag))
  }

  def markAsReservedAction(
      ts: Vector[SpendingInfoDb]): DBIOAction[Vector[
                                                SpendingInfoDb
                                              ],
                                              NoStream,
                                              Effect.Read & Effect.Write] = {
    // 1. Check if any are reserved already
    // 2. if not, reserve them
    // 3. if they are reserved, throw an exception?
    val outPoints = ts.map(_.outPoint)
    table
      .filter(_.outPoint.inSet(outPoints))
      .filter(
        _.state.inSet(TxoState.receivedStates)
      ) // must be available to reserve
      .map(_.state)
      .update(TxoState.Reserved)
      .flatMap { count =>
        if (count != ts.length) {
          val exn = new RuntimeException(
            s"Failed to reserve all utxos, expected=${ts.length} actual=$count"
          )
          DBIO.failed(exn)
        } else {
          DBIO.successful(count)
        }
      }
      .map(_ => ts.map(_.copyWithState(TxoState.Reserved)))
  }

  def markAsReserved(
      ts: Vector[SpendingInfoDb]
  ): Future[Vector[SpendingInfoDb]] = {
    safeDatabase.run(markAsReservedAction(ts))
  }

  def hasDuplicates(): Future[Boolean] = FutureUtil.makeAsync { () =>
    withStatement(
      s"SELECT EXISTS (SELECT tx_outpoint, COUNT(*) FROM $fullTableName GROUP BY tx_outpoint HAVING COUNT(*) > 1)"
    ) { st =>
      val rs = st.executeQuery()
      try {
        if (rs.next()) {
          rs.getBoolean(1)
        } else false
      } finally rs.close()
    }
  }

  private def fullTableName: String = {
    val tableNode = table.toNode
      .asInstanceOf[TableExpansion]
      .table
      .asInstanceOf[TableNode]
    tableNode.schemaName
      .map(schema => "\"" + schema + "\".")
      .getOrElse("") + tableNode.tableName
  }

  private def withStatement[T](sql: String)(f: PreparedStatement => T): T = {
    val conn = database.source.createConnection()
    val autoCommit = conn.getAutoCommit
    conn.setAutoCommit(true)
    try {
      val st = conn.prepareStatement(sql)
      try f(st)
      finally st.close()
    } finally {
      conn.setAutoCommit(autoCommit)
      conn.close()
    }
  }

  private def findScriptPubKeysAction(
      ids: Seq[Long]
  ): DBIOAction[Map[Long, ScriptPubKeyDb], NoStream, Effect.Read] = {
    val query = spkTable.filter(t => t.id.inSet(ids))
    query.result.map { action =>
      action.map { case spk =>
        (spk.id.get, spk)
      }.toMap
    }
  }

  private def findScriptPubKeys(
      ids: Seq[Long]
  ): Future[Map[Long, ScriptPubKeyDb]] = {
    val action = findScriptPubKeysAction(ids)
    safeDatabase.run(action)
  }

  private def findScriptPubKeysAction(
      utxos: Vector[UTXORecord]
  ): DBIOAction[Map[Long, ScriptPubKeyDb], NoStream, Effect.Read] = {
    val ids = utxos.map(_.scriptPubKeyId)
    findScriptPubKeysAction(ids)
  }

  private def findScriptPubKeysByUtxos(
      utxos: Vector[UTXORecord]
  ): Future[Map[Long, ScriptPubKeyDb]] = {
    val action = findScriptPubKeysAction(utxos)
    safeDatabase.run(action)
  }

  private def findPublicKeyScriptsBySpendingInfoDb(
      spendingInfoDbs: Seq[SpendingInfoDb]
  ): Future[Map[ScriptPubKey, Long]] = {
    val spks = spendingInfoDbs.map(_.output.scriptPubKey)
    val query = spkTable.filter(t => t.scriptPubKey.inSet(spks))
    safeDatabase
      .runVec(query.result)
      .map(_.map(spk => (spk.scriptPubKey, spk.id.get)).toMap)
  }

  /** This table stores the necessary information to spend a transaction output
    * (TXO) at a later point in time. It also stores how many confirmations it
    * has, whether or not it is spent (i.e. if it is a UTXO or not) and the TXID
    * of the transaction that created this output.
    */
  case class SpendingInfoTable(tag: Tag)
      extends TableAutoInc(tag, schemaName, "txo_spending_info") {

    def outPoint: Rep[TransactionOutPoint] =
      column("tx_outpoint", O.Unique)

    def txid: Rep[DoubleSha256DigestBE] = column("txid")

    def state: Rep[TxoState] = column("txo_state")

    def scriptPubKeyId: Rep[Long] = column("script_pub_key_id")

    def value: Rep[CurrencyUnit] = column("value")

    def privKeyPath: Rep[HDPath] = column("hd_privkey_path")

    def redeemScriptOpt: Rep[Option[ScriptPubKey]] =
      column("redeem_script")

    def scriptWitnessOpt: Rep[Option[ScriptWitness]] = column("script_witness")

    def spendingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] = column(
      "spending_txid"
    )

    /** All UTXOs must have a SPK in the wallet that gets spent to */
    def fk_scriptPubKeyId: slick.lifted.ForeignKeyQuery[?, ScriptPubKeyDb] = {
      val scriptPubKeyTable = spkTable
      foreignKey(
        "fk_scriptPubKeyId",
        sourceColumns = scriptPubKeyId,
        targetTableQuery = scriptPubKeyTable
      )(_.id)
    }

    /** All UTXOs must have a corresponding transaction in the wallet */
    def fk_incoming_txId
        : slick.lifted.ForeignKeyQuery[?, IncomingTransactionDb] = {
      foreignKey(
        "fk_incoming_txId",
        sourceColumns = txid,
        targetTableQuery = txTable
      )(_.txIdBE)
    }

    private val fromTuple: (
        (
            TransactionOutPoint,
            DoubleSha256DigestBE,
            TxoState,
            Long,
            CurrencyUnit,
            HDPath,
            Option[ScriptPubKey],
            Option[ScriptWitness],
            Option[DoubleSha256DigestBE],
            Option[Long]
        )
    ) => UTXORecord = {
      case (
            outpoint,
            _,
            state,
            scriptPubKeyId,
            value,
            path,
            redeemScriptOpt,
            scriptWitOpt,
            spendingTxIdOpt,
            idOpt
          ) =>
        UTXORecord(
          outpoint,
          state,
          scriptPubKeyId,
          value,
          path,
          redeemScriptOpt,
          scriptWitOpt,
          spendingTxIdOpt,
          idOpt
        )
    }

    private val toTuple: UTXORecord => Option[
      (
          TransactionOutPoint,
          DoubleSha256DigestBE,
          TxoState,
          Long,
          CurrencyUnit,
          HDPath,
          Option[ScriptPubKey],
          Option[ScriptWitness],
          Option[DoubleSha256DigestBE],
          Option[Long]
      )
    ] = { case utxo: UTXORecord =>
      Some(
        (
          utxo.outpoint,
          utxo.outpoint.txIdBE,
          utxo.state,
          utxo.scriptPubKeyId,
          utxo.value,
          utxo.path,
          utxo.redeemScript,
          utxo.scriptWitness,
          utxo.spendingTxIdOpt,
          utxo.id
        )
      )
    }

    override def * = {
      (
        outPoint,
        txid,
        state,
        scriptPubKeyId,
        value,
        privKeyPath,
        redeemScriptOpt,
        scriptWitnessOpt,
        spendingTxIdOpt,
        id.?
      ).<>(fromTuple, toTuple)
    }
  }
}
