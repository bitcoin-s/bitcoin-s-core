import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._

object Deps {

  object V {
    val antV = "1.10.14"
    val bouncyCastle = "1.78"
    val dropwizardMetricsV = "4.2.25" //https://github.com/dropwizard/metrics

    val logback = "1.5.4" //https://github.com/qos-ch/logback
    val log4jV = "1.2.17"

    val logkitV = "20020529"
    val avalonLoggingV = "20020627"

    val scalacheck = "1.17.0"
    val scalaTest = "3.2.18" //https://www.scalatest.org/

    val scalaTestPlus =
      "3.2.18.0" //super annoying... https://oss.sonatype.org/content/groups/public/org/scalatestplus/
    val slf4j = "2.0.12"
    val spray = "1.3.6"
    val zeromq = "0.5.4"
    val scalapb = "0.11.15"
    val akkav = "1.0.1"
    val playv = "3.0.2" //https://github.com/playframework/play-json/releases
    val akkaStreamv = "1.0.2"
    val jUnixSocketV = "2.9.1"
    val scodecV = "1.1.38"
    val junitV = "0.13.3"
    val nativeLoaderV = "2.5.0"
    val typesafeConfigV = "1.4.3"

    val asyncNewScalaV = "1.0.1"

    val flywayV =
      "9.2.1" //https://flywaydb.org/documentation/learnmore/releaseNotes
    val postgresV = "42.7.3" //https://jdbc.postgresql.org/
    val akkaActorV = akkaStreamv

    val slickV = "3.5.0"
    val sqliteV = "3.45.2.0" //https://github.com/xerial/sqlite-jdbc

    val scalameterV = "0.17"
    val scalamockV = "6.0.0"
    val scalaCollectionCompatV = "2.11.0"
    val pgEmbeddedV = "1.0.3"

    val breezeV = "1.3"

    val newMicroPickleV = "3.1.4" //https://github.com/com-lihaoyi/upickle/
    val newMicroJsonV = newMicroPickleV

    val osgiFrameworkV = "1.10.0"
    val osgiJdbcV = "1.1.0"
    val osgiCoreV = "8.0.0"

    // akka-http-upickle is not yet published
    // to Maven central. There's a PR for adding
    // suport, https://github.com/hseeberger/akka-http-json/pull/314.
    // Until that's merged, you'll have to pull down
    // that PR, do `sbt publishLocal` and replace the
    // value here with whatever is in there. This
    // obviously has to be changed before this is
    // merged.

    val scalaJsStubsV = "1.1.0"
    // CLI deps
    val scoptV = "4.1.0"
    val sttpV = "3.9.5" // https://github.com/softwaremill/sttp
    val codehausV = "3.1.12"
    val scalaJsTimeV = "2.5.0"
    val zxingV = "3.5.0"

    val monixV = "3.4.1"

    val javaxServletV = "4.0.1"
    val javaxJmsV = "2.0.1"
    val javaxMailV = "1.4.7"

    val gsonV = "2.10.1"
    val jnaV = "5.14.0"
    val waffleJnaV = "3.3.0"
  }

  object Compile {

    val bouncycastle =
      "org.bouncycastle" % "bcprov-jdk18on" % V.bouncyCastle withSources () withJavadoc ()

    val scodec =
      Def.setting(
        "org.scodec" %%% "scodec-bits" % V.scodecV withSources () withJavadoc ())

    val slf4j =
      "org.slf4j" % "slf4j-api" % V.slf4j % "provided" withSources () withJavadoc ()

    val zeromq =
      "org.zeromq" % "jeromq" % V.zeromq withSources () withJavadoc ()

    val scalapb =
      "com.thesamet.scalapb" %% "scalapb-runtime" % V.scalapb % "protobuf"

    val akkaHttp =
      "org.apache.pekko" %% "pekko-http" % V.akkav withSources () withJavadoc ()

    /*    val akkaHttp2 =
      "org.apache.pekko" %% "pekko-http2-support" % V.akkav withSources () withJavadoc ()*/

    val akkaStream =
      "org.apache.pekko" %% "pekko-stream" % V.akkaStreamv withSources () withJavadoc ()

    val akkaDiscovery =
      "org.apache.pekko" %% "pekko-discovery" % V.akkaStreamv withSources () withJavadoc ()

    val akkaActor =
      "org.apache.pekko" %% "pekko-actor" % V.akkaStreamv withSources () withJavadoc ()

    val akkaSlf4j =
      "org.apache.pekko" %% "pekko-slf4j" % V.akkaStreamv withSources () withJavadoc ()

    val akkaTestkit =
      "org.apache.pekko" %% "pekko-testkit" % V.akkaActorV withSources () withJavadoc ()

    val pekkoGrpc =
      "org.apache.pekko" %% "pekko-grpc-runtime" % V.akkaStreamv withSources () withJavadoc ()

    //https://mvnrepository.com/artifact/org.apache.ant/ant/
    val ant = "org.apache.ant" % "ant" % V.antV

    val gson =
      "com.google.code.gson" % "gson" % V.gsonV //https://github.com/google/gson

    val jUnixSocket =
      "com.kohlschutter.junixsocket" % "junixsocket-core" % V.jUnixSocketV

    lazy val arch = System.getProperty("os.arch")

    lazy val osName = System.getProperty("os.name") match {
      case n if n.startsWith("Linux") => "linux"
      case n if n.startsWith("Mac") =>
        if (arch == "aarch64") {
          //needed to accommodate the different chip
          //arch for M1
          s"mac-${arch}"
        } else {
          "mac"
        }
      case n if n.startsWith("Windows") => "win"
      case x                            => throw new Exception(s"Unknown platform $x!")
    }

    val javaxServlet =
      "javax.servlet" % "javax.servlet-api" % V.javaxServletV // https://mvnrepository.com/artifact/javax.servlet/javax.servlet-api
    val javaxJms =
      "javax.jms" % "javax.jms-api" % V.javaxJmsV // https://mvnrepository.com/artifact/javax.jms/javax.jms-api
    val javaxMail =
      "javax.mail" % "mail" % V.javaxMailV // https://mvnrepository.com/artifact/javax.mail/mail

    val jna = "net.java.dev.jna" % "jna" % V.jnaV
    val waffleJna = "com.github.waffle" % "waffle-jna" % V.waffleJnaV

    val playJson =
      "org.playframework" %% "play-json" % V.playv withSources () withJavadoc ()

    val typesafeConfig =
      "com.typesafe" % "config" % V.typesafeConfigV withSources () withJavadoc ()

    val logback =
      "ch.qos.logback" % "logback-classic" % V.logback withSources () withJavadoc ()

    val log4j =
      "log4j" % "log4j" % V.log4jV //https://github.com/apache/commons-logging/blob/0d4f2604ada038fd95e714d504d2278f1bd5814a/pom.xml#L486
    val logkit =
      "logkit" % "logkit" % V.logkitV //https://github.com/apache/commons-logging/blob/0d4f2604ada038fd95e714d504d2278f1bd5814a/pom.xml#L492
    val avalonLogging =
      "avalon-framework" % "avalon-framework" % V.avalonLoggingV //https://github.com/apache/commons-logging/blob/0d4f2604ada038fd95e714d504d2278f1bd5814a/pom.xml#L498

    val codehaus = "org.codehaus.janino" % "janino" % V.codehausV

    //for loading secp256k1 natively
    val nativeLoader =
      "org.scijava" % "native-lib-loader" % V.nativeLoaderV withSources () withJavadoc ()

    //node deps
    val slick =
      "com.typesafe.slick" %% "slick" % V.slickV withSources () withJavadoc ()
    val slickHikari = "com.typesafe.slick" %% "slick-hikaricp" % V.slickV
    val sqlite = "org.xerial" % "sqlite-jdbc" % V.sqliteV
    val postgres = "org.postgresql" % "postgresql" % V.postgresV
    val flyway = "org.flywaydb" % "flyway-core" % V.flywayV

    val newMicroJson = "com.lihaoyi" %% "ujson" % V.newMicroJsonV

    val newMicroPickle =
      Def.setting("com.lihaoyi" %%% "upickle" % V.newMicroPickleV)

    val osgiFramework =
      "org.osgi" % "org.osgi.framework" % V.osgiFrameworkV // https://mvnrepository.com/artifact/org.osgi/org.osgi.framework,
    val osgiJdbc =
      "org.osgi" % "org.osgi.service.jdbc" % V.osgiJdbcV // https://mvnrepository.com/artifact/org.osgi/org.osgi.service.jdbc
    val osgiCore =
      "org.osgi" % "osgi.core" % V.osgiCoreV // https://mvnrepository.com/artifact/org.osgi/osgi.core,

    // parsing of CLI opts and args
    val scopt = "com.github.scopt" %% "scopt" % V.scoptV

    // HTTP client lib
    val sttp = "com.softwaremill.sttp.client3" %% "core" % V.sttpV
    //https://sttp.softwaremill.com/en/stable/backends/wrappers/logging.html#using-slf4j
    val sttpSlf4j = "com.softwaremill.sttp.client3" %% "slf4j-backend" % V.sttpV

    val scalaCollectionCompat =
      "org.scala-lang.modules" %% "scala-collection-compat" % V.scalaCollectionCompatV

    val scalacheck =
      Def.setting(
        "org.scalacheck" %%% "scalacheck" % V.scalacheck withSources () withJavadoc ())

    val scalaJsStubs =
      "org.scala-js" % "scalajs-stubs_2.13" % V.scalaJsStubsV % "provided"

    val scalaJsTime =
      Def.setting(
        "io.github.cquiroz" %%% "scala-java-time" % V.scalaJsTimeV withSources () withJavadoc ())

    val scalaTest =
      Def.setting(
        "org.scalatest" %%% "scalatest" % V.scalaTest withSources () withJavadoc ())

    val scalaTestPlus = Def.setting(
      "org.scalatestplus" %%% "scalacheck-1-17" % V.scalaTestPlus withSources () withJavadoc ())

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV withSources () withJavadoc ()

    val dropwizardMetricsCore =
      "io.dropwizard.metrics" % "metrics-core" % V.dropwizardMetricsV withSources () withJavadoc ()

    val dropwizardMetricsHealthChecks =
      "io.dropwizard.metrics" % "metrics-healthchecks" % V.dropwizardMetricsV

    val dropwizardMetricsJvm =
      "io.dropwizard.metrics" % "metrics-jvm" % V.dropwizardMetricsV // https://mvnrepository.com/artifact/io.dropwizard.metrics/metrics-jvm

    val monixExecution =
      Def.setting(
        "io.monix" %%% "monix-execution" % V.monixV withSources () withJavadoc ())
  }

  object Test {

    val newAsync =
      "org.scala-lang.modules" %% "scala-async" % V.asyncNewScalaV % "test" withSources () withJavadoc ()

    val junitInterface =
      "com.github.sbt" % "junit-interface" % V.junitV % "test" withSources () withJavadoc ()
    val logback = Compile.logback % "test"

    val scalacheck = Def.setting(
      "org.scalacheck" %%% "scalacheck" % V.scalacheck % "test" withSources () withJavadoc ())

    val scalaTest = Def.setting(
      "org.scalatest" %%% "scalatest" % V.scalaTest % "test" withSources () withJavadoc ())
    val scalaMock = "org.scalamock" %% "scalamock" % V.scalamockV

    val spray =
      "io.spray" %% "spray-json" % V.spray % "test" withSources () withJavadoc ()

    val akkaHttpTestkit =
      "org.apache.pekko" %% "pekko-http-testkit" % V.akkav % "test" withSources () withJavadoc ()

    val akkaStream =
      "org.apache.pekko" %% "pekko-stream-testkit" % V.akkaStreamv % "test" withSources () withJavadoc ()
    val playJson = Compile.playJson % "test"

    val scalameter =
      "com.storm-enroute" %% "scalameter" % V.scalameterV % "test" withSources () withJavadoc ()

    val scalaCollectionCompat =
      "org.scala-lang.modules" %% "scala-collection-compat" % V.scalaCollectionCompatV

    val pgEmbedded =
      "com.opentable.components" % "otj-pg-embedded" % V.pgEmbeddedV % "test" withSources () withJavadoc ()

    val akkaTestkit =
      "org.apache.pekko" %% "pekko-testkit" % V.akkaActorV withSources () withJavadoc ()
  }

  def asyncUtils = Def.setting {
    Vector(Compile.monixExecution.value)
  }

  val chain = List(
    Compile.logback
  )

  val chainTest = List(
    Test.pgEmbedded
  )

  val appCommons = Def.setting {
    List(
      Compile.newMicroPickle.value,
      Compile.playJson,
      Compile.slf4j,
      Compile.typesafeConfig
    )
  }

  def core = Def.setting {
    List(
      Compile.bouncycastle,
      Compile.scodec.value
    )
  }

  val cryptoJVM = List(
    Compile.bouncycastle,
    Compile.scalaJsStubs
  )

  def crypto: Def.Initialize[Seq[ModuleID]] = {
    Def.setting {
      List(
        Compile.scodec.value
      )
    }
  }

  val dlcWallet =
    List(
      Compile.newMicroJson
    )

  val dlcNode =
    List(
      Compile.newMicroJson,
      Compile.akkaActor
    )

  val dlcNodeTest =
    List(
      Test.akkaTestkit
    )

  val dlcWalletTest =
    List(
      Test.akkaTestkit,
      Test.pgEmbedded
    )

  val secp256k1jni = List(
    Compile.nativeLoader,
    Test.junitInterface
  )

  def coreTest = Def.setting {
    List(
      Test.junitInterface,
      Test.scalaTest.value,
      Test.scalaCollectionCompat,
      Compile.newMicroPickle.value
    )
  }

  val coreTestJVM = Def.setting {
    List(
      Test.junitInterface,
      Test.scalaTest.value,
      Test.scalaCollectionCompat
    )
  }

  val coreJs = Def.setting {
    List(
      Compile.scalaJsTime.value
    )
  }

  def cryptoTest = Def.setting {
    List(
      Test.scalaTest.value,
      Test.scalacheck.value,
      Compile.scalaTestPlus.value
    )
  }

  def bitcoindZmq = Def.setting {
    List(
      Compile.zeromq,
      Compile.slf4j,
      Test.logback,
      Test.scalacheck.value,
      Test.scalaTest.value
    )
  }

  val bitcoindRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.typesafeConfig,
    Compile.slf4j
  )

  def bitcoindRpcTest = Def.setting {
    List(
      Test.akkaHttpTestkit,
      Test.akkaStream,
      Test.logback,
      Test.scalaTest.value,
      Test.scalacheck.value,
      Test.newAsync,
      Test.scalaCollectionCompat
    )
  }

  val bench = List(
    "org.slf4j" % "slf4j-api" % V.slf4j withSources () withJavadoc (),
    Compile.logback
  )

  def dbCommons = Def.setting {
    List(
      Compile.dropwizardMetricsCore,
      Compile.flyway,
      Compile.slick,
      Compile.logback,
      Compile.sqlite,
      Compile.postgres,
      Compile.slickHikari,
      Compile.slf4j,
      Test.scalaTest.value,
      Test.pgEmbedded
    )
  }

  val cli = Def.setting {
    List(
      Compile.sttp,
      Compile.slf4j,
      Compile.sttpSlf4j,
      Compile.newMicroPickle.value,
      Compile.scopt,
      Compile.logback,
      Compile.codehaus,
      Compile.ant
    )
  }

  val server = Def.setting {
    Vector(
      Compile.newMicroPickle.value,
      Compile.logback,
      Compile.akkaActor,
      Compile.akkaHttp,
      Compile.akkaStream,
      Compile.akkaSlf4j
    ) ++ appServerTransitiveDeps
  }

  /** Transitive dependencies needed for the oracleServer to build properly with jlink */
  private val serverTransitiveDeps = Vector(
    //transitive deps needed for jlink
    Compile.codehaus,
    Compile.gson,
    Compile.dropwizardMetricsHealthChecks,
    Compile.dropwizardMetricsJvm,
    //postgres transitive deps
    Compile.jna,
    Compile.waffleJna,
    Compile.osgiCore,
    Compile.osgiJdbc,
    Compile.osgiFramework,
    //logging transitive deps
    Compile.log4j,
    Compile.avalonLogging,
    Compile.logkit,
    //transitive javax deps
    Compile.javaxServlet,
    Compile.javaxMail,
    Compile.javaxJms
  )

  private val appServerTransitiveDeps = serverTransitiveDeps
  private val oracleServerTransitiveDeps = serverTransitiveDeps

  val oracleServer = Def.setting {
    Vector(
      Compile.newMicroPickle.value,
      Compile.logback,
      Compile.akkaActor,
      Compile.akkaHttp,
      Compile.akkaSlf4j
    ) ++ oracleServerTransitiveDeps
  }

  val eclairRpc = List(
    Compile.akkaHttp,
    Compile.akkaStream,
    Compile.playJson,
    Compile.slf4j
  )

  val clightningRpc = List(
    Compile.jUnixSocket,
    Compile.playJson
  )

  val clightningRpcTest = Def.setting {
    List(
      Test.logback,
      Test.scalaTest.value,
      Test.scalacheck.value
    )
  }

  val tor: Def.Initialize[List[ModuleID]] = Def.setting {
    List(
      Compile.akkaStream,
      Compile.akkaHttp,
      Compile.scodec.value
    )
  }

  val lndRpc = List(
    Compile.scalapb,
    Compile.akkaHttp,
    /*    Compile.akkaHttp2,*/
    Compile.akkaStream,
    Compile.akkaDiscovery,
    Compile.pekkoGrpc,
    Compile.playJson,
    Compile.slf4j
  )

  def eclairRpcTest = Def.setting {
    List(
      Test.akkaHttpTestkit,
      Test.akkaStream,
      Test.logback,
      Test.scalaTest.value,
      Test.scalacheck.value
    )
  }

  def feeProvider = Def.setting {
    List(
      Compile.akkaHttp,
      Compile.akkaActor,
      Compile.akkaStream
    )
  }

  def feeProviderTest = Def.setting {
    List(
      Test.akkaTestkit,
      Test.scalaTest.value
    )
  }

  val esplora = Def.setting {
    List(
      Compile.akkaHttp,
      Compile.akkaActor,
      Compile.akkaStream
    )
  }

  val esploraTest = Def.setting {
    List(
      Test.akkaTestkit,
      Test.scalaTest.value
    )
  }

  val node = List(
    Compile.akkaActor,
    Compile.logback,
    Compile.slick,
    Compile.slickHikari,
    Compile.sqlite,
    Compile.slf4j
  )

  val nodeTest = Def.setting {
    List(
      Test.akkaTestkit,
      Test.scalaTest.value,
      Test.pgEmbedded
    )
  }

  def testkitCore = Def.setting {
    List(
      Compile.newMicroPickle.value,
      Compile.scalaCollectionCompat,
      Compile.scalacheck.value,
      Compile.scalaTest.value,
      Compile.scalaTestPlus.value
    )
  }

  def testkit = Def.setting {
    List(
      Compile.scalacheck.value,
      Compile.scalaTest.value,
      Compile.scalaTestPlus.value,
      Compile.pgEmbedded,
      Compile.slf4j,
      Compile.akkaTestkit
    )
  }

  def keyManager(scalaVersion: String) =
    List(
      Compile.newMicroJson
    )

  val keyManagerTest = List(
    Compile.slf4j,
    Test.logback
  )

  def wallet(scalaVersion: String) =
    List(
      Compile.newMicroJson,
      Compile.logback,
      Compile.slf4j,
      Compile.akkaActor,
      Compile.akkaStream
    )

  val walletTest = List(
    Test.pgEmbedded
  )

  def docs = Def.setting {
    List(
      Compile.logback,
      Test.scalaTest.value,
      Test.logback
    )
  }

  val walletServerTest = List(
    Compile.typesafeConfig,
    Test.scalaMock,
    Test.akkaHttpTestkit,
    Test.akkaStream
  )

  val dlcOracle =
    List(
      Compile.newMicroJson,
      Compile.logback,
      Compile.slf4j
    )

  val dlcOracleTest =
    List(
      Compile.newMicroJson,
      Compile.logback
    )

  val serverRoutes = List(
    Compile.akkaHttp,
    Compile.akkaActor,
    Compile.akkaSlf4j,
    Compile.akkaStream,
    Compile.slf4j
  )
}
