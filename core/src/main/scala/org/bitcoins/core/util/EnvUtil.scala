package org.bitcoins.core.util

import scala.util.Properties

object EnvUtil {
  lazy val osName: String = System.getProperty("os.name")

  lazy val isLinux: Boolean = osName.startsWith("Linux")

  lazy val isMac: Boolean = osName.startsWith("Mac")

  lazy val isWindows: Boolean = osName.startsWith("Windows")

  lazy val isCI: Boolean = Properties.envOrNone("CI").contains("true")

  def getVersion: String = getClass.getPackage.getImplementationVersion

  def getJdkVersion: String = System.getProperty("java.version")

  def isNativeSecp256k1Disabled: Boolean = {
    val secpDisabled = System.getenv("DISABLE_SECP256K1")
    secpDisabled != null && (secpDisabled.toLowerCase == "true" || secpDisabled == "1")
  }

  /** Parses the number of commits since last tag Expects a string of format
    * 1.9.0-9-eddcc94b-SNAPSHOT or 1.9.0
    *
    * If it's a release like 1.9.0, this will return None
    */
  def parseCommitsSinceLastTag(version: String): Option[Int] = {
    val split = version.split("-")
    if (split.length == 1) {
      // means this is a release
      None
    } else {
      Some(split(1).toInt)
    }
  }

  /** Parses the version number from a string of format
    * 1.9.0-9-eddcc94b-SNAPSHOT
    *
    * This method will return "1.9.0"
    */
  def parseVersion(version: String): String = {
    val split = version.split("-")
    split.head
  }
}
