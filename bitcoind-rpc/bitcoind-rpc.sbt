import java.nio.file.Files
import java.security.MessageDigest
import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.util.Properties

name := "bitcoin-s-bitcoind-rpc"

libraryDependencies ++= Deps.bitcoindRpc

CommonSettings.prodSettings

TaskKeys.downloadBitcoind := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("bitcoind")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for bitcoind binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val versions =
    List("28.1", "27.2", "26.1")

  logger.debug(
    s"(Maybe) downloading Bitcoin Core binaries for versions: ${versions.mkString(",")}")

  def getPlatformAndSuffix(version: String): (String, String) = {
    if (Properties.isLinux) ("x86_64-linux-gnu", "tar.gz")
    else if (Properties.isMac) {
      if (versions.contains(version)) {
        if (System.getProperty("os.arch") == "aarch64")
          ("arm64-apple-darwin", "tar.gz")
        else ("x86_64-apple-darwin", "tar.gz")
      } else ("osx64", "tar.gz")
    } else if (Properties.isWin) ("win64", "zip")
    else sys.error(s"Unsupported OS: ${Properties.osName}")
  }

  implicit val ec = scala.concurrent.ExecutionContext.global
  val downloads = Future.traverse(versions) { version =>
    val (platform, suffix) = getPlatformAndSuffix(version)
    val archiveLocation = binaryDir resolve s"$version.$suffix"
    val location =
      if (version.init.endsWith("rc")) { // if it is a release candidate
        val (base, rc) = version.splitAt(version.length - 3)
        s"https://bitcoincore.org/bin/bitcoin-core-$base/test.$rc/bitcoin-$version-$platform.$suffix"
      } else
        s"https://bitcoincore.org/bin/bitcoin-core-$version/bitcoin-$version-$platform.$suffix"

    val expectedEndLocation = binaryDir resolve s"bitcoin-$version"

    if (
      Files
        .list(binaryDir)
        .iterator
        .asScala
        .map(_.toString)
        .exists(expectedEndLocation.toString.startsWith(_))
    ) {
      logger.debug(
        s"Directory $expectedEndLocation already exists, skipping download of version $version")
      Future.unit
    } else {
      // copy of FutureUtil.makeAsync
      def makeAsync(func: () => Unit): Future[Unit] = {
        val resultP = Promise[Unit]()

        ec.execute { () =>
          val result: Unit = func()
          resultP.success(result)
        }

        resultP.future
      }

      makeAsync { () =>
        logger.info(
          s"Downloading bitcoind version $version from location: $location")
        logger.info(s"Placing the file in $archiveLocation")
        val downloadCommand = url(location) #> archiveLocation.toFile
        downloadCommand.!!

        val bytes = Files.readAllBytes(archiveLocation)
        val hash = MessageDigest
          .getInstance("SHA-256")
          .digest(bytes)
          .map("%02x" format _)
          .mkString

        val expectedHash =
          if (Properties.isLinux)
            Map(
              "26.1" -> "a5b7d206384a8100058d3f2e2f02123a8e49e83f523499e70e86e121a4897d5b",
              "27.2" -> "acc223af46c178064c132b235392476f66d486453ddbd6bca6f1f8411547da78",
              "28.1" -> "07f77afd326639145b9ba9562912b2ad2ccec47b8a305bd075b4f4cb127b7ed7"
            )
          else if (Properties.isMac)
            Map(
              "26.1" -> (if (System.getProperty("os.arch") == "aarch64")
                "8a8e415763b7ffd5988153cf03967d812eca629016dd3b0ddf6da3ab6f4a3621"
              else
                "acb50edd20692a9d023de12da573b64ca0fd9b4e9a2b88d1251020a3022b0f27"),
              "27.2" -> (if (System.getProperty("os.arch") == "aarch64")
                "8f2247f4786f3559d37189b58452c91623efc5fa6886c975fa9386f9ff3f1001"
              else
                "e1efd8c4605b2aabc876da93b6eee2bedd868ce7d1f02b0220c1001f903b3e2c"),
            "28.1" -> (if (System.getProperty("os.arch") == "aarch64")
              "abf4d2f7ebda6284e2246bce3591bcf161c114e370c0443ccc049b2728dc7e20"
            else
              "e1efd8c4605b2aabc876da93b6eee2bedd868ce7d1f02b0220c1001f903b3e2c")
            )
          else if (Properties.isWin)
            Map(
              "26.1" -> "7bd0849e47472aeff99a0ea2c0cefd98f5be829e5a2d3b0168b5a54456cc638a",
              "27.2" -> "82e18f768aa5962b3c002d7f5d6ec9338896804f48406af4b5054c927575dbdf",
              "28.1" -> "2d636ad562b347c96d36870d6ed810f4a364f446ca208258299f41048b35eab0"
            )
          else sys.error(s"Unsupported OS: ${Properties.osName}")

        val success = hash.equalsIgnoreCase(expectedHash(version))
        if (success) {
          logger.info(s"Download complete and verified, unzipping result")
          val cmds = Vector(
            "tar",
            "-xzf",
            archiveLocation.toString,
            "--directory",
            binaryDir.toString
          )
          //val extractCommand = s"""tar -xzf \"$archiveLocation\" --directory \"$binaryDir\""""
          logger.info(s"Extracting archive with command: $cmds")
          cmds.!!
        } else {
          Files.deleteIfExists(expectedEndLocation)
          logger.error(
            s"Downloaded invalid version of bitcoind v$version, got $hash, expected ${expectedHash(version)}")
        }

        logger.info(s"Deleting archive")
        Files.delete(archiveLocation)

        if (!success)
          throw new RuntimeException(s"Failed to download bitcoind v$version")
      }

    }
  }

  //timeout if we cannot download in 5 minutes
  Await.result(downloads, 2.minutes)
}
