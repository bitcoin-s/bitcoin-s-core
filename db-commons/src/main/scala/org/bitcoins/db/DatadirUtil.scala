package org.bitcoins.db

import org.bitcoins.commons.file.FileUtil

import java.io.IOException
import java.nio.file.{Files, Path}
import scala.util.Try

object DatadirUtil {

  def zipDatadir(source: Path, target: Path): Try[Path] = Try {
    if (Files.exists(target)) {
      throw new IOException(
        s"Cannot zip datadir. Target file already exists: $target")
    }
    val temp = Files.createTempDirectory(source, "backup")
    try {
      // we don't want to store chaindb.sqlite as these databases are huge
      // skip logs and binaries as these can be large as well
      val tempRE = (".*/" + temp.getFileName + "/.*").r

      FileUtil.copyDirectory(
        source = source,
        target = temp,
        fileNameFilter = Vector(
          ".*.sqlite$".r,
          ".*.sqlite-shm$".r,
          ".*.sqlite-wal$".r,
          ".*bitcoin-s.log$".r,
          ".*/seeds/.*".r,
          ".*/tor/.*".r,
          ".*/binaries/.*".r,
          ".*.zip$".r,
          ".*.DS_Store".r,
          tempRE
        )
      )

      SQLiteUtil.backupDirectory(source = source,
                                 target = temp,
                                 fileNameFilter =
                                   Vector(".*chaindb.sqlite$".r, tempRE))

      FileUtil.zipDirectory(
        source = temp,
        target = target
      )
    } finally {
      FileUtil.removeDirectory(temp)
      ()
    }
  }

}
