import java.nio.file._
import java.security.MessageDigest
import scala.util.Properties

name := "bitcoin-s-eclair-rpc"

libraryDependencies ++= Deps.eclairRpc

CommonSettings.prodSettings

TaskKeys.downloadEclair := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("eclair")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for Eclair binaires: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val version = "0.11.0"
  val commit = "7e7ad45"

  logger.debug(s"(Maybe) downloading Eclair binaries for version: $version")

  val versionDir = binaryDir resolve version
  val location =
    s"https://github.com/ACINQ/eclair/releases/download/v$version/eclair-node-$version-$commit-bin.zip"

  if (Files.exists(versionDir)) {
    logger.debug(
      s"Directory $versionDir already exists, skipping download of Eclair $version")
  } else {
    logger.info(s"Creating directory $version")
    Files.createDirectories(versionDir)

    val archiveLocation = versionDir resolve s"eclair-node-$version-$commit.zip"
    logger.info(
      s"Downloading Eclair $version from location: $location, to destination: $archiveLocation")
    (url(location) #> archiveLocation.toFile).!!

    val bytes = Files.readAllBytes(archiveLocation)
    val hash = MessageDigest
      .getInstance("SHA-256")
      .digest(bytes)
      .map("%02x" format _)
      .mkString

    val expectedHash =
      "e0efea6be7a3c3252e9e7f7181647ffbf67fb286939290e6088c341ee07994f8"
    val success = hash.equalsIgnoreCase(expectedHash)
    if (success) {
      logger.info(s"Download complete and verified, unzipping result")
      val cmds = Vector(
        "unzip",
        archiveLocation.toString,
        "-d",
        versionDir.toString
      )
      logger.info(s"Extracting archive with command: $cmds")
      cmds.!!
    } else {
      Files.deleteIfExists(versionDir)
      logger.error(
        s"Downloaded invalid version of eclair, got $hash, expected $expectedHash")
    }

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)

    if (!success) {
      throw new RuntimeException(s"Failed to download eclair v$version")
    }

    logger.info(s"Download complete")
  }
}
