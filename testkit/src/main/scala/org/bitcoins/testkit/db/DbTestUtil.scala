package org.bitcoins.testkit.db

import org.apache.pekko.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.db._
import org.bitcoins.db.models.MasterXPubDAO
import scodec.bits.ByteVector
import slick.lifted.ProvenShape

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}

object DbTestUtil {

  def createTestDbManagement(
      testAppConfig: TestAppConfig
  )(implicit system: ActorSystem): TestDbManagement = {
    new TestDbManagement with JdbcProfileComponent[TestAppConfig] {
      override val ec: ExecutionContext = system.dispatcher

      override def appConfig: TestAppConfig = testAppConfig
    }
  }
}

trait TestDbManagement extends DbManagement {
  self: JdbcProfileComponent[TestAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val testTable: TableQuery[Table[?]] =
    TestDAO()(ec, appConfig).table

  private lazy val masterXpubTable: TableQuery[Table[?]] = {
    MasterXPubDAO()(ec = ec, appConfig = appConfig).table
  }

  override lazy val allTables: List[TableQuery[Table[?]]] =
    List(testTable, masterXpubTable)

}

case class TestAppConfig(baseDatadir: Path, configOverrides: Vector[Config])(
    implicit override val ec: ExecutionContext
) extends DbAppConfig
    with TestDbManagement
    with JdbcProfileComponent[TestAppConfig] {

  override protected[bitcoins] def moduleName: String = TestAppConfig.moduleName

  override protected[bitcoins] type ConfigType = TestAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Vector[Config]
  ): TestAppConfig =
    TestAppConfig(baseDatadir, configs)

  override def appConfig: TestAppConfig = this

  override def start(): Future[Unit] = {
    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }
    for {
      _ <- super.start()
      _ = logger.debug(s"Initializing test setup")
      _ <- createTable(TestDAO()(ec, this).table)
    } yield ()
  }
}

object TestAppConfig {
  val moduleName: String = "test"
}

case class TestDb(pk: String, data: ByteVector)

case class TestDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: TestAppConfig
) extends CRUD[TestDb, String]
    with SlickUtil[TestDb, String] {

  import profile.api._

  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[TestTable] = TableQuery[TestTable]

  override def createAll(ts: Vector[TestDb]): Future[Vector[TestDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ts: Vector[String]
  ): Query[TestTable, TestDb, Seq] = {
    table.filter(_.pk.inSet(ts))
  }

  override def findByPrimaryKey(t: String): Query[TestTable, TestDb, Seq] = {
    table.filter(_.pk === t)
  }

  override def findAll(ts: Vector[TestDb]): Query[TestTable, TestDb, Seq] =
    findByPrimaryKeys(ts.map(_.pk))

  class TestTable(tag: Tag) extends Table[TestDb](tag, "test_table") {

    def pk: Rep[String] = column[String]("pk", O.PrimaryKey)

    def data: Rep[ByteVector] = column[ByteVector]("data")

    def * : ProvenShape[TestDb] =
      (pk, data).<>(TestDb.apply, TestDb.unapply)
  }
}
