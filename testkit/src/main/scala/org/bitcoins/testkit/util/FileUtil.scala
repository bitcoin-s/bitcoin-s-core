package org.bitcoins.testkit.util

import org.bitcoins.commons.util.BitcoinSLogger

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.util.{Properties, Random}

object FileUtil extends BitcoinSLogger {

  /** Returns a `BufferedSource` for any file on the classpath */
  def getFileAsSource(fileName: String): scala.io.BufferedSource = {
    scala.io.Source.fromURL(getClass.getResource(s"/$fileName"))
  }

  /** Deletes the given temporary directory
    *
    * @throws IllegalArgumentException
    *   if the given directory isn't in the user temp dir location
    */
  def deleteTmpDir(dir: File): Boolean = {
    val isTemp = dir.getPath `startsWith` Properties.tmpDir
    if (!isTemp) {
      logger.warn(
        s"Directory $dir is not in the system temp dir location! You most likely didn't mean to delete this directory."
      )
      false
    } else if (!dir.isDirectory) {
      dir.delete()
    } else {
      val filesOpt = Option(dir.listFiles())
      filesOpt match {
        case Some(files) =>
          files.foreach(deleteTmpDir)
        case None =>
        // do nothing since list files must have returned null
      }
      dir.delete()
    }
  }

  def deleteTmpDir(path: Path): Boolean = {
    deleteTmpDir(path.toFile)
  }

  @tailrec
  final def randomDirName: String = {
    val dirname = 0.until(5).map(_ => Random.alphanumeric.head).mkString
    val dir = new File(dirname)
    if (!dir.exists()) {
      dirname
    } else {
      randomDirName
    }
  }

  def tmpDir(): File = {
    val f = Paths.get(Properties.tmpDir, FileUtil.randomDirName).toFile
    f.mkdirs()
    f
  }

  def withTempDir[T](prefix: String)(f: Path => T): T = {
    val dir = Files.createTempDirectory(prefix)
    try {
      f(dir)
    } finally {
      deleteTmpDir(dir)
      ()
    }
  }

}
