package org.bitcoins.db

import org.bitcoins.commons.util.BitcoinSLogger

import java.io.IOException
import java.nio.file.{Files, LinkOption, Path}
import scala.util.matching.Regex

object SQLiteUtil extends BitcoinSLogger {

  Class.forName("org.sqlite.JDBC")

  def pathToJdbcUrl(path: Path): String = s"jdbc:sqlite:${path.toAbsolutePath}"

  def backupDirectory(
      source: Path,
      target: Path,
      fileNameFilter: Vector[Regex] = Vector.empty,
      dbExtension: String = ".sqlite"): Unit = {
    Files.list(source).toArray.map(_.asInstanceOf[Path]).foreach { file =>
      if (
        fileNameFilter.exists(reg =>
          file.toAbsolutePath.toString.matches(reg.regex))
      ) {
        logger.info(s"Skipping ${file.toAbsolutePath} for backup")
      } else {
        if (Files.isDirectory(file)) {
          backupDirectory(file,
                          target.resolve(file.getFileName),
                          fileNameFilter,
                          dbExtension)
        } else if (
          Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) && file.toString
            .endsWith(dbExtension)
        ) {
          val backupFile = target.resolve(file.getFileName)
          logger.info(
            s"Backing up file=${file.toAbsolutePath} to ${backupFile.toAbsolutePath}")
          Files.createDirectories(target)
          backup(pathToJdbcUrl(file), backupFile)
          logger.info(s"Done backing up file=${file.toAbsolutePath}")
        }
      }
    }
  }

  def backup(jdbcUrl: String, backupFilePath: Path): Unit = {
    val conn = java.sql.DriverManager.getConnection(jdbcUrl)
    try {
      val existsAndWritable =
        Files.exists(backupFilePath) && Files.isWritable(backupFilePath)
      val doesntExistAndPArentIsWritable = !Files.exists(
        backupFilePath) && Files.isWritable(backupFilePath.getParent)
      if (existsAndWritable || doesntExistAndPArentIsWritable) {
        val _ =
          conn.createStatement().executeUpdate(s"BACKUP TO $backupFilePath")
      } else {
        throw new IOException(
          s"Backup destination is not writable: $backupFilePath")
      }
    } finally conn.close()
  }

  def setJournalMode(jdbcUrl: String, mode: String): Unit = {
    val conn = java.sql.DriverManager.getConnection(jdbcUrl)
    try {
      val _ =
        conn
          .createStatement()
          .executeUpdate(s"PRAGMA journal_mode=${mode.toUpperCase}")
    } finally conn.close()
  }

  def createDbFileIfDNE(dbPath: Path, dbName: String): Unit = {
    if (!Files.exists(dbPath)) {
      val _ = {
        logger.debug(s"Creating database directory=${dbPath}")
        Files.createDirectories(dbPath)
        val dbFilePath = dbPath.resolve(dbName)
        logger.debug(s"Creating database file=$dbFilePath")
        Files.createFile(dbFilePath)
      }
    }
  }
}
