package org.bitcoins.feeprovider

import org.bitcoins.crypto.StringFactory

sealed abstract class FeeProviderName

object FeeProviderName extends StringFactory[FeeProviderName] {

  case object BitcoinerLive extends FeeProviderName

  case object BitGo extends FeeProviderName

  case object Constant extends FeeProviderName

  case object MempoolSpace extends FeeProviderName

  val all: Vector[FeeProviderName] =
    Vector(BitcoinerLive, BitGo, Constant, MempoolSpace)

  override def fromStringOpt(str: String): Option[FeeProviderName] = {
    all.find(_.toString.toLowerCase == str.toLowerCase)
  }

  override def fromString(string: String): FeeProviderName = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find FeeProviderName for string=$string")
    }
  }
}
