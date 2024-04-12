package org.bitcoins.core.psbt

import org.bitcoins.crypto.StringFactory

abstract class PSBTRole {
  def shortName: String
  def order: Int
}

/** The different roles of operations that can be preformed on a PSBT
  * [[https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#roles]]
  */
object PSBTRole extends StringFactory[PSBTRole] {

  case object CreatorPSBTRole extends PSBTRole {
    override def shortName: String = "creator"
    override def order: Int = 0
  }

  case object UpdaterPSBTRole extends PSBTRole {
    override def shortName: String = "updater"
    override def order: Int = 1
  }

  case object SignerPSBTRole extends PSBTRole {
    override def shortName: String = "signer"
    override def order: Int = 2
  }

  case object FinalizerPSBTRole extends PSBTRole {
    override def shortName: String = "finalizer"
    override def order: Int = 3
  }

  case object ExtractorPSBTRole extends PSBTRole {
    override def shortName: String = "extractor"
    override def order: Int = 4
  }

  val all: Vector[PSBTRole] = Vector(CreatorPSBTRole,
                                     UpdaterPSBTRole,
                                     SignerPSBTRole,
                                     FinalizerPSBTRole,
                                     ExtractorPSBTRole)

  override def fromStringOpt(string: String): Option[PSBTRole] = {
    all.find(_.toString.toLowerCase == string.toLowerCase)
  }

  override def fromString(string: String): PSBTRole = {
    fromStringOpt(string) match {
      case Some(role) => role
      case None =>
        sys.error(s"Could not find PSBT role for string=$string")
    }
  }
}
