package org.bitcoins.commons.util

import org.bitcoins.core.util.FutureUtil

import java.io.File
import scala.concurrent.{ExecutionContext, Future}
import scala.sys.process.{Process, ProcessBuilder, ProcessLogger}

/** A trait that helps start bitcoind/eclair when it is started via bitcoin-s */
trait NativeProcessFactory extends BitcoinSLogger {
  implicit protected def executionContext: ExecutionContext

  private var processOpt: Option[Process] = None

  private lazy val process: ProcessBuilder = scala.sys.process.Process(cmd)

  /** The command to start the daemon on the underlying OS */
  def cmd: String

  def isAlive(): Boolean = {
    processOpt match {
      case Some(p) =>
        p.isAlive()
      case None =>
        false
    }
  }

  /** Starts the binary by spinning up a new process */
  def startBinary(): Future[Unit] = FutureUtil.makeAsync { () =>
    processOpt match {
      case Some(p) =>
        //don't do anything as it is already started
        logger.info(s"Binary was already started! process=$p")
        ()
      case None =>
        if (cmd.nonEmpty) {
          val started = process.run(NativeProcessFactory.processLogger)
          processOpt = Some(started)
        } else {
          logger.warn("cmd not set, no binary started")
        }
        ()
    }
  }

  /** Stops the binary by destroying the underlying operating system process
    *
    * If the client is a remote client (not started on the host operating system)
    * this method is a no-op
    */
  def stopBinary(): Future[Unit] = FutureUtil.makeAsync { () =>
    processOpt match {
      case Some(process) =>
        if (process.isAlive()) {
          val _ = process.destroy()
        }
        processOpt = None
      case None =>
        logger.info(s"No process found, binary wasn't started!")
        //no process running, nothing to do
        ()
    }
  }

}

object NativeProcessFactory extends BitcoinSLogger {

  val processLogger: ProcessLogger =
    ProcessLogger(logger.info(_), logger.error(_))

  def findExecutableOnPath(name: String): Option[File] =
    sys.env
      .getOrElse("PATH", "")
      .split(File.pathSeparator)
      .map(directory => new File(directory, name))
      .find(file => file.isFile && file.canExecute)
}
