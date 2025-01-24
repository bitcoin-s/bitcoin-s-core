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
    List("28.0", "27.1", "26.1")

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
  val downloads = versions.map { version =>
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
              "27.1" -> "c9840607d230d65f6938b81deaec0b98fe9cb14c3a41a5b13b2c05d044a48422",
              "28.0" -> "7fe294b02b25b51acb8e8e0a0eb5af6bbafa7cd0c5b0e5fcbb61263104a82fbc"
            )
          else if (Properties.isMac)
            Map(
              "26.1" -> (if (System.getProperty("os.arch") == "aarch64")
                           "8a8e415763b7ffd5988153cf03967d812eca629016dd3b0ddf6da3ab6f4a3621"
                         else
                           "acb50edd20692a9d023de12da573b64ca0fd9b4e9a2b88d1251020a3022b0f27"),
              "27.1" -> (if (System.getProperty("os.arch") == "aarch64")
                           "ad4a3fd484077224a82dd56d194efb6e614467f413ab1dfb8776da4d08a4c227"
                         else
                           "e1efd8c4605b2aabc876da93b6eee2bedd868ce7d1f02b0220c1001f903b3e2c"),
              "28.0" -> (if (System.getProperty("os.arch") == "aarch64")
                           "c8108f30dfcc7ddffab33f5647d745414ef9d3298bfe67d243fe9b9cb4df4c12"
                         else
                           "e1efd8c4605b2aabc876da93b6eee2bedd868ce7d1f02b0220c1001f903b3e2c")
            )
          else if (Properties.isWin)
            Map(
              "26.1" -> "7bd0849e47472aeff99a0ea2c0cefd98f5be829e5a2d3b0168b5a54456cc638a",
              "27.1" -> "9719871a2c9a45c741e33d670d2319dcd3f8f52a6059e9c435a9a2841188b932",
              "28.0" -> "85282f4ec1bcb0cfe8db0f195e8e0f6fb77cfbe89242a81fff2bc2e9292f7acf"
            )
          else sys.error(s"Unsupported OS: ${Properties.osName}")

        val success = hash.equalsIgnoreCase(expectedHash(version))
        if (success) {
          logger.info(s"Download complete and verified, unzipping result")

          val extractCommand =
            s"tar -xzf $archiveLocation --directory $binaryDir"
          logger.info(s"Extracting archive with command: $extractCommand")
          extractCommand.!!
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

  // timeout if we cannot download in 5 minutes
  Await.result(Future.sequence(downloads), 5.minutes)
}
