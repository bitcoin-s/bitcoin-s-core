package org.bitcoins.db.models

import org.bitcoins.core.crypto.{ChainCode, ExtKeyPubVersion, ExtPublicKey}
import org.bitcoins.core.number.{UInt32, UInt8}
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.db.{CRUD, DbAppConfig, SlickUtil}
import scodec.bits.ByteVector

import java.sql.SQLException
import scala.concurrent.{ExecutionContext, Future}

case class ExtPublicKeyDTO(
    version: ExtKeyPubVersion,
    depth: UInt8,
    fingerprint: ByteVector,
    child: UInt32,
    chainCode: ChainCode,
    publicKey: ECPublicKey,
    name: Option[String]
) {

  def toExtPublicKey: ExtPublicKey =
    ExtPublicKey(version, depth, fingerprint, child, chainCode, publicKey)

}

/** The primary key type is the public key associated with the extended public
  * key [[ExtPublicKey.key]]
  */
case class MasterXPubDAO()(implicit
    ec: ExecutionContext,
    appConfig: DbAppConfig)
    extends CRUD[ExtPublicKeyDTO, ECPublicKey]
    with SlickUtil[ExtPublicKeyDTO, ECPublicKey] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: TableQuery[MasterXpubTable] = TableQuery[MasterXpubTable]

  def create(
      extPublicKey: ExtPublicKey,
      name: Option[String] = None): Future[ExtPublicKeyDTO] = {
    val dto = createDTO(extPublicKey, name)
    create(dto)
  }

  override def createAllAction(ts: Vector[ExtPublicKeyDTO])
      : profile.api.DBIOAction[Vector[ExtPublicKeyDTO],
                               profile.api.NoStream,
                               Effect.Write] = {
    val fixedSqlAction = table ++= ts

    fixedSqlAction.map(_ => ts)
  }

  override def create(t: ExtPublicKeyDTO): Future[ExtPublicKeyDTO] = {
    val recordCount = table.size.result

    val action = recordCount.flatMap {
      case 0 =>
        table += t
      case count @ _ =>
        DBIO.failed(
          new SQLException(s"Only 1 master xpub should be stored, got=$count"))
    }

    database.run(action).map(_ => t)
  }

  override def createAll(
      extKeys: Vector[ExtPublicKeyDTO]): Future[Vector[ExtPublicKeyDTO]] = {
    if (extKeys.size != 1)
      Future.failed(
        new SQLException(
          s"Only 1 master xpub should be stored, got=${extKeys.size}"))
    else
      create(extKeys.head).map(Vector(_))
  }

  override protected def findByPrimaryKeys(pubkeys: Vector[ECPublicKey])
      : profile.api.Query[profile.api.Table[ExtPublicKeyDTO],
                          ExtPublicKeyDTO,
                          Seq] = {
    table.filter(_.key.inSet(pubkeys))
  }

  override protected def findAll(ts: Vector[ExtPublicKeyDTO])
      : profile.api.Query[profile.api.Table[ExtPublicKeyDTO],
                          ExtPublicKeyDTO,
                          Seq] = {
    findByPrimaryKeys(ts.map(_.publicKey))
  }

  /** Validates the stored public key against the given xpub
    * @throws RuntimeException
    *   if the keys are different
    */
  def validate(xpub: ExtPublicKey): Future[Unit] = {
    findAll().map { xpubs =>
      require(xpubs.length == 1,
              s"Only 1 master xpub should be stored, got=${xpubs.length}")
      if (xpub != xpubs.head.toExtPublicKey) {
        sys.error(
          s"Keymanager xpub and stored xpub are different, stored=${xpubs.head.toExtPublicKey}, keymanager=${xpub}")
      }
    }
  }

  def existsOneXpub(): Future[Boolean] = {
    count().map(_ == 1)
  }

  def findXPub(): Future[ExtPublicKeyDTO] = {
    findAll().map { xpubs =>
      require(xpubs.length == 1,
              s"Only 1 master xpub should be stored, got=${xpubs.length}")
      xpubs.head
    }
  }

  def updateName(name: String): Future[Unit] = {
    val recordCount = table.size.result

    val action = recordCount.flatMap {
      case 1 =>
        table.map(_.name).update(Option(name))
      case count @ _ =>
        DBIO.failed(
          new SQLException(s"Only 1 master xpub should be stored, got=$count"))
    }

    database.run(action).map(_ => ())
  }

  class MasterXpubTable(tag: Tag)
      extends Table[ExtPublicKeyDTO](tag, schemaName, "master_xpub") {

    def version = column[ExtKeyPubVersion]("extpubkey_version")

    def depth = column[UInt8]("depth")

    def fingerprint = column[ByteVector]("fingerprint")

    def childNum = column[UInt32]("childnum")

    def chaincode = column[ChainCode]("chaincode")

    def key = column[ECPublicKey]("key", O.PrimaryKey, O.Unique)

    def name = column[Option[String]]("name")

    def * = {
      (version, depth, fingerprint, childNum, chaincode, key, name).<>(
        ExtPublicKeyDTO.apply,
        ExtPublicKeyDTO.unapply)
    }
  }

  private def createDTO(
      epk: ExtPublicKey,
      name: Option[String]): ExtPublicKeyDTO =
    ExtPublicKeyDTO(
      version = epk.version,
      depth = epk.depth,
      fingerprint = epk.fingerprint,
      child = epk.childNum,
      chainCode = epk.chainCode,
      publicKey = epk.key,
      name = name
    )

}
