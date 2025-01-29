import java.nio.file._
import java.security.MessageDigest
import scala.util.Properties

name := "bitcoin-s-clightning-rpc"

libraryDependencies ++= Deps.clightningRpc

CommonSettings.prodSettings

TaskKeys.downloadCLightning := {
  val logger = streams.value.log
  import scala.sys.process._

  val binaryDir = CommonSettings.binariesPath.resolve("clightning")

  if (Files.notExists(binaryDir)) {
    logger.info(s"Creating directory for clightning binaries: $binaryDir")
    Files.createDirectories(binaryDir)
  }

  val version = "24.11.1"

  val (platform, suffix) =
    if (Properties.isLinux) {
      //from: https://stackoverflow.com/a/51614324/967713
      val processBuilder = new java.lang.ProcessBuilder("lsb_release", "-rs")
      val inputStream = new java.io.InputStreamReader(processBuilder.start().getInputStream())
      val version = new java.io.BufferedReader(inputStream).readLine()
      if (version == "22.04")  {
        ("Ubuntu-22.04-amd64", "tar.xz")
      } else if (version == "24.04") {
        ("Ubuntu-24.04-amd64", "tar.xz")
      } else {
        ("Ubuntu-20.04", "tar.xz")
      }
    } else {
      sys.error(s"Unsupported OS: ${Properties.osName}")
    }

  logger.debug(s"(Maybe) downloading clightning binaries for version: $version")

  val versionDir = binaryDir resolve version
  val location =
    s"https://github.com/ElementsProject/lightning/releases/download/v$version/clightning-v$version-$platform.tar.xz"

  if (Files.exists(versionDir)) {
    logger.debug(
      s"Directory $versionDir already exists, skipping download of clightning $version")
  } else {
    Files.createDirectories(versionDir)
    val archiveLocation = binaryDir resolve s"$version.$suffix"
    logger.info(
      s"Downloading clightning version $version from location: $location")
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
      if (platform == "Ubuntu-20.04-amd64") {
        "0068852306bca9df3d213c6a29bb90451eb538be83e413d6838e9e2d2729ff7f"
      } else if (platform == "Ubuntu-22.04-amd64") {
        "7d78e49615ace6ff8ee9ebfdf30e108ecf41ce98834493260ee31486389b781f"
      } else if (platform == "Ubuntu-24.04-amd64") {
        ""
      }
      else sys.error(s"Unsupported OS: ${Properties.osName}")

    val success = hash.equalsIgnoreCase(expectedHash)
    if (hash.equalsIgnoreCase(expectedHash)) {
      logger.info(s"Download complete and verified, unzipping result")

      val extractCommand = s"tar -xf $archiveLocation --directory $versionDir"
      logger.info(s"Extracting archive with command: $extractCommand")
      extractCommand.!!
    } else {
      logger.error(
        s"Downloaded invalid version of c-lightning, got $hash, expected $expectedHash")
    }

    logger.info(s"Deleting archive")
    Files.delete(archiveLocation)

    if (!success) {
      throw new RuntimeException(
        s"Failed to download and verify clightning v$version")
    }
  }
}
