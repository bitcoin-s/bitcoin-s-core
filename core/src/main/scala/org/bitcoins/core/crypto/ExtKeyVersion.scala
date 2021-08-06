package org.bitcoins.core.crypto

import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits._

sealed abstract class ExtKeyVersion extends NetworkElement

sealed abstract class ExtKeyPrivVersion extends ExtKeyVersion

sealed abstract class ExtKeyPubVersion extends ExtKeyVersion

/** @see [[https://github.com/satoshilabs/slips/blob/master/slip-0132.md#registered-hd-version-bytes SLIP132]]
  * for a list of registered HD version bytes
  */
object ExtKeyVersion extends Factory[ExtKeyVersion] {

  private[bitcoins] val allPrivs: Vector[ExtKeyPrivVersion] = Vector(
    LegacyMainNetPriv,
    LegacyTestNet3Priv,
    SegWitMainNetPriv,
    SegWitTestNet3Priv,
    NestedSegWitMainNetPriv,
    NestedSegWitTestNet3Priv
  )

  private[bitcoins] val all: Vector[ExtKeyVersion] =
    allPrivs ++ ExtKeyPubVersion.allPubs

  override def fromBytes(bytes: ByteVector): ExtKeyVersion = {
    fromBytesOpt(bytes) match {
      case Some(v) => v
      case None =>
        sys.error(s"Cannot parse bytes to ExtKeyVersion, got=$bytes")
    }
  }

  override def fromBytesOpt(bytes: ByteVector): Option[ExtKeyVersion] =
    all.find(_.bytes == bytes)

  /** Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `xprv`
    */
  case object LegacyMainNetPriv extends ExtKeyPrivVersion {
    override val bytes = hex"0x0488ADE4"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `tprv`
    */
  case object LegacyTestNet3Priv extends ExtKeyPrivVersion {
    override val bytes = hex"0x04358394"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `zprv`
    */
  case object SegWitMainNetPriv extends ExtKeyPrivVersion {
    override val bytes = hex"0x04b2430c"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `vprv`
    */
  case object SegWitTestNet3Priv extends ExtKeyPrivVersion {
    override val bytes = hex"0x045f18bc"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `yprv`
    *
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L39 Samourai Wallet]]
    */
  case object NestedSegWitMainNetPriv extends ExtKeyPrivVersion {
    override val bytes = hex"0x049D7878"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `uprv`
    *
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L40 Samourai wallet]]
    */
  case object NestedSegWitTestNet3Priv extends ExtKeyPrivVersion {
    override val bytes = hex"0x044a4e28"
  }
}

object ExtKeyPubVersion extends Factory[ExtKeyPubVersion] {

  private[bitcoins] val allPubs: Seq[ExtKeyPubVersion] = Vector(
    LegacyMainNetPub,
    LegacyTestNet3Pub,
    SegWitMainNetPub,
    SegWitTestNet3Pub,
    NestedSegWitMainNetPub,
    NestedSegWitTestNet3Pub
  )

  /** Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `xpub`
    */
  case object LegacyMainNetPub extends ExtKeyPubVersion {
    override val bytes = hex"0x0488b21E"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `upub`
    *
    * @see [[https://github.com/trezor/trezor-firmware/blob/master/common/defs/bitcoin/bitcoin_testnet.json#L17 Trezor]]
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L33 Samourai wallet]]
    */
  case object NestedSegWitTestNet3Pub extends ExtKeyPubVersion {
    // Value stolen from Trezor lib, see link above
    // ByteVector.fromLong(71979618).toHex = 00000000044a5262
    override val bytes = hex"0x044a5262"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `ypub`
    *
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L32 Samourai Wallet]]
    */
  case object NestedSegWitMainNetPub extends ExtKeyPubVersion {
    override val bytes = hex"0x049D7CB2"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `vpub`
    */
  case object SegWitTestNet3Pub extends ExtKeyPubVersion {
    override val bytes = hex"0x045f1cf6"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `tpub`
    */
  case object LegacyTestNet3Pub extends ExtKeyPubVersion {
    override val bytes = hex"0x043587CF"
  }

  /** Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `zpub`
    */
  case object SegWitMainNetPub extends ExtKeyPubVersion {
    override val bytes = hex"0x04b24746"
  }

  override def fromBytes(bytes: ByteVector): ExtKeyPubVersion = {
    allPubs.find(_.bytes == bytes) match {
      case Some(v) => v
      case None =>
        sys.error(s"Could not find ExtKeyPubVersion from bytes=$bytes")
    }
  }
}
