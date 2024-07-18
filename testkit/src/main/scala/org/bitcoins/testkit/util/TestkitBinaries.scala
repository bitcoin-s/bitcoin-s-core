package org.bitcoins.testkit.util

import org.bitcoins.commons.config.AppConfig

import java.nio.file.{Path}

object TestkitBinaries {

  private val base: Path = AppConfig.DEFAULT_BITCOIN_S_DATADIR
    .resolve("binaries")

  /** The base directory where binaries needed in tests are located.
    */
  lazy val baseBinaryDirectory: Path = base
}
