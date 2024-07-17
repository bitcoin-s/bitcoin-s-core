package org.bitcoins.testkit.util

import org.bitcoins.commons.config.AppConfig

import java.nio.file.{Path, Paths}
import scala.util.Properties

object TestkitBinaries {

  private val base: Path = AppConfig.DEFAULT_BITCOIN_S_DATADIR
    .resolve("binaries")

  /** The base directory where binaries needed in tests are located.
    */
  lazy val baseBinaryDirectory: Path = {
    val home = Paths.get(Properties.userHome)
    fromRoot(home)
  }

  /** Gives you an arbitrary root path, and then tacks on .bitcoin-s/binaries/
    * onto the end of it
    */
  private def fromRoot(path: Path): Path = {
    path.resolve(base)
  }
}
