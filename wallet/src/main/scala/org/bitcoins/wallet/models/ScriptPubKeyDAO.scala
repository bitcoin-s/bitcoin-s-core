package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.ScriptPubKeyDb
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future}

case class ScriptPubKeyDAO()(implicit
    ec: ExecutionContext,
    config: WalletAppConfig
) extends CRUDAutoInc[ScriptPubKeyDb] {

  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[ScriptPubKeyTable] =
    TableQuery[ScriptPubKeyTable]

  /** Creates a new row in the database only if the given SPK (not ID) does not
    * exists.
    */
  def createIfNotExists(spkDb: ScriptPubKeyDb): Future[ScriptPubKeyDb] = {
    val spkFind = table.filter(_.scriptPubKey === spkDb.scriptPubKey).result
    val actions = for {
      spkOpt <- spkFind.headOption
      spk <- spkOpt match {
        case Some(foundSpk) =>
          DBIO.successful(foundSpk)
        case None =>
          for {
            newSpkId <- (table returning table.map(_.id)) += spkDb
          } yield {
            spkDb.copyWithId(newSpkId)
          }
      }
    } yield spk

    safeDatabase.run(actions)
  }

  /** Finds a scriptPubKey in the database, if it exists */
  def findScriptPubKey(spk: ScriptPubKey): Future[Option[ScriptPubKeyDb]] = {
    val foundVecF = findScriptPubKeys(Vector(spk))
    foundVecF.map(_.headOption)
  }

  def findScriptPubKeysAction(
      spks: Vector[ScriptPubKey]
  ): DBIOAction[Vector[ScriptPubKeyDb], NoStream, Effect.Read] = {
    val hashes = spks.map(ScriptPubKeyDb.hash)
    // group hashes to avoid https://github.com/bitcoin-s/bitcoin-s/issues/4220
    val groupedHashes: Vector[Vector[Sha256Digest]] =
      hashes.grouped(1000).toVector
    val actions =
      groupedHashes.map(hashes => table.filter(_.hash.inSet(hashes)).result)
    DBIO.sequence(actions).map(_.flatten.toVector)
  }

  /** Searches for the given set of spks and returns the ones that exist in the
    * db
    */
  def findScriptPubKeys(
      spks: Vector[ScriptPubKey]
  ): Future[Vector[ScriptPubKeyDb]] = {
    val action = findScriptPubKeysAction(spks)
    safeDatabase.run(action)
  }

  class ScriptPubKeyTable(tag: Tag)
      extends TableAutoInc(tag, schemaName, "pub_key_scripts") {

    def scriptPubKey: Rep[ScriptPubKey] = column("script_pub_key")

    def scriptType: Rep[ScriptType] = column("script_type")

    def hash: Rep[Sha256Digest] = column("hash")

    private type ScriptPubKeyTuple =
      (Option[Long], ScriptPubKey, ScriptType, Sha256Digest)

    private val fromTuple: ScriptPubKeyTuple => ScriptPubKeyDb = {
      case (id, scriptPubKey, scriptType, hash) =>
        require(
          scriptPubKey.scriptType == scriptType,
          s"script type must match it script: `${scriptPubKey.scriptType}` != `${scriptType}` "
        )
        ScriptPubKeyDb(scriptPubKey, hash, id)
    }

    private val toTuple: ScriptPubKeyDb => Option[ScriptPubKeyTuple] = {
      scriptPubKeyDb =>
        Some(
          (
            scriptPubKeyDb.id,
            scriptPubKeyDb.scriptPubKey,
            scriptPubKeyDb.scriptPubKey.scriptType,
            scriptPubKeyDb.hash
          )
        )
    }

    override def * =
      (id.?, scriptPubKey, scriptType, hash).<>(fromTuple, toTuple)
  }

}
