package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.{UInt5, UInt64, UInt8}
import org.bitcoins.core.protocol.ln.currency.{LnCurrencyUnit, PicoBitcoins}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln.util.LnUtil
import org.bitcoins.core.util._
import org.bitcoins.crypto.{
  CryptoUtil,
  ECPrivateKey,
  Sha256Digest,
  StringFactory
}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

sealed abstract class LnInvoice {

  require(timestamp < LnInvoice.MAX_TIMESTAMP_U64,
          s"timestamp ${timestamp.toBigInt} < ${LnInvoice.MAX_TIMESTAMP}")

  require(
    isValidSignature,
    s"Did not receive a valid digital signature for the invoice $toString")

  def hrp: LnHumanReadablePart

  def timestamp: UInt64

  def lnTags: LnTaggedFields

  def signature: LnInvoiceSignature

  private def data: Vector[UInt5] = {
    val ts = LnInvoice.uInt64ToBase32(timestamp)
    val u5s: Vector[UInt5] = ts ++ lnTags.data ++ signature.data
    u5s
  }

  def network: LnParams = hrp.network

  def amount: Option[LnCurrencyUnit] = hrp.amount

  def amountPicoBitcoins: Option[PicoBitcoins] = {
    amount.map(_.toPicoBitcoins)
  }

  /** The [[org.bitcoins.core.protocol.ln.node.NodeId NodeId]] that we are paying this invoice too
    * We can either recover this with public key recovery from
    * the [[org.bitcoins.core.protocol.ln.LnInvoiceSignature LnInvoiceSignature]] or if
    * [[org.bitcoins.core.protocol.ln.LnTag.NodeIdTag LnTag.NodeIdTag]] is
    * defined we MUST use that NodeId, as per
    * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#requirements-3 BOLT11]]
    */
  def nodeId: NodeId = {

    if (lnTags.nodeId.isDefined) {
      lnTags.nodeId.get.nodeId
    } else {
      val recoverId = signature.recoverId

      val hashMsg = sigHash
      val (pubKey1, pubKey2) =
        CryptoUtil.recoverPublicKey(signature.signature, hashMsg.bytes)

      if (recoverId.toInt % 2 == 0) {
        NodeId(pubKey1)
      } else {
        NodeId(pubKey2)
      }
    }

  }

  /** The data that is hashed and then signed in the
    * [[org.bitcoins.core.protocol.ln.LnInvoiceSignature LnInvoiceSignature]]
    */
  def signatureData: ByteVector = {
    val sig = LnInvoice.buildSignatureData(hrp, timestamp, lnTags)
    sig
  }

  /** The hash that is signed by the [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]] corresponding
    * to the `nodeId`
    */
  private def sigHash: Sha256Digest = {
    val hash = CryptoUtil.sha256(signatureData)
    hash
  }

  def bech32Checksum: String = {
    val bytes: Vector[UInt5] = LnInvoice.createChecksum(hrp, data)
    val bech32 = Bech32.encode5bitToString(bytes)
    bech32
  }

  def isValidSignature: Boolean = {
    Try(nodeId.pubKey.verify(sigHash, signature.signature)).getOrElse(false)
  }

  override def toString: String = {
    val b = new StringBuilder
    b.append(hrp.toString)
    b.append('1')

    val dataToString = Bech32.encode5bitToString(data)
    b.append(dataToString)
    b.append(bech32Checksum)

    b.toString()
  }
}

object LnInvoice extends StringFactory[LnInvoice] {

  private case class LnInvoiceImpl(
      hrp: LnHumanReadablePart,
      timestamp: UInt64,
      lnTags: LnTaggedFields,
      signature: LnInvoiceSignature)
      extends LnInvoice

  val MAX_TIMESTAMP: BigInt = NumberUtil.pow2(35)

  val MAX_TIMESTAMP_U64: UInt64 = UInt64(MAX_TIMESTAMP)

  def decodeTimestamp(u5s: Vector[UInt5]): UInt64 = {
    val decoded = LnUtil.decodeNumber(u5s.toList)
    UInt64(decoded)
  }

  def hrpExpand(lnHumanReadablePart: LnHumanReadablePart): Vector[UInt5] =
    lnHumanReadablePart.expand

  def createChecksum(
      hrp: LnHumanReadablePart,
      data: Vector[UInt5]): Vector[UInt5] = {
    val hrpBytes = hrpExpand(hrp)
    val u5s = Bech32.createChecksum(hrpBytes ++ data, Bech32Encoding.Bech32)
    u5s
  }

  def verifyChecksum(hrp: LnHumanReadablePart, u5s: Seq[UInt5]): Boolean = {
    val data = hrpExpand(hrp) ++ u5s
    val checksum = Bech32.polyMod(data)
    checksum == 1
  }

  def apply(hrp: LnHumanReadablePart, data: Vector[UInt5]): LnInvoice = {

    //https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#data-part
    val TIMESTAMP_LEN = 7
    val SIGNATURE_LEN = 104
    val MIN_LENGTH = TIMESTAMP_LEN + SIGNATURE_LEN
    if (data.length < MIN_LENGTH) {
      throw new IllegalArgumentException(
        s"Cannot create invoice with data length less then $MIN_LENGTH, got ${data.length}")
    } else {
      //first 35 bits is time stamp
      val timestampU5s = data.take(TIMESTAMP_LEN)

      val timestamp = decodeTimestamp(timestampU5s)

      //last bits should be a 520 bit signature
      //should be 104 5 bit increments (104 * 5 = 520)
      val signatureU5s = data.takeRight(SIGNATURE_LEN)
      val signature = LnInvoiceSignature.fromU5s(signatureU5s)

      val tags = data.slice(TIMESTAMP_LEN, data.length - SIGNATURE_LEN)

      val taggedFields = LnTaggedFields.fromUInt5s(tags)

      LnInvoice(hrp = hrp,
                timestamp = timestamp,
                lnTags = taggedFields,
                signature = signature)
    }

  }

  override def fromString(bech32String: String): LnInvoice = {
    val sepIndexes = {
      bech32String.zipWithIndex.filter { case (sep, _) =>
        sep == Bech32.separator
      }
    }
    if (sepIndexes.isEmpty) {
      throw new IllegalArgumentException(
        "LnInvoice did not have the correct separator")
    } else {
      val (_, sepIndex) = sepIndexes.last

      val hrp = bech32String.take(sepIndex)

      val (_, data) = bech32String.splitAt(sepIndex + 1)

      if (hrp.length < 1) {
        throw new IllegalArgumentException("HumanReadablePart is too short")
      } else if (data.length < 6) {
        throw new IllegalArgumentException("Data part is too short")
      } else {

        val hrpValid = LnHumanReadablePart.fromStringT(hrp)

        val dataValid = Bech32.checkDataValidity(data)

        val isChecksumValid: Try[Vector[UInt5]] = hrpValid.flatMap {
          (h: LnHumanReadablePart) =>
            dataValid.flatMap { (d: Vector[UInt5]) =>
              stripChecksumIfValid(h, d)
            }
        }

        val invoiceT = isChecksumValid.flatMap { (d: Vector[UInt5]) =>
          hrpValid.map(h => LnInvoice(h, d))
        }

        invoiceT match {
          case Success(i)   => i
          case Failure(exn) => throw exn
        }
      }
    }
  }

  def apply(
      hrp: LnHumanReadablePart,
      timestamp: UInt64,
      lnTags: LnTaggedFields,
      signature: LnInvoiceSignature): LnInvoice = {
    LnInvoiceImpl(hrp = hrp,
                  timestamp = timestamp,
                  lnTags = lnTags,
                  signature = signature)
  }

  def apply(
      hrp: LnHumanReadablePart,
      timestamp: UInt64,
      lnTags: LnTaggedFields,
      privateKey: ECPrivateKey): LnInvoice = {

    val signature = buildLnInvoiceSignature(hrp, timestamp, lnTags, privateKey)
    LnInvoiceImpl(hrp = hrp,
                  timestamp = timestamp,
                  lnTags = lnTags,
                  signature = signature)
  }

  def buildSignatureData(
      hrp: LnHumanReadablePart,
      timestamp: UInt64,
      lnTags: LnTaggedFields): ByteVector = {
    val tsu5 = uInt64ToBase32(timestamp)
    val payloadU5 = tsu5 ++ lnTags.data
    val payloadU8 = Bech32.from5bitTo8bit(payloadU5, pad = true)
    val payload = UInt8.toBytes(payloadU8)
    hrp.bytes ++ payload
  }

  def buildSigHashData(
      hrp: LnHumanReadablePart,
      timestamp: UInt64,
      lnTags: LnTaggedFields): Sha256Digest = {
    val sigdata = buildSignatureData(hrp, timestamp, lnTags)
    CryptoUtil.sha256(sigdata)
  }

  def buildLnInvoiceSignature(
      hrp: LnHumanReadablePart,
      timestamp: UInt64,
      lnTags: LnTaggedFields,
      privateKey: ECPrivateKey): LnInvoiceSignature = {
    val sigHash = buildSigHashData(hrp, timestamp, lnTags)
    val sig = privateKey.sign(sigHash)

    val (pub1, _) = CryptoUtil.recoverPublicKey(sig, sigHash.bytes)
    val recoveryId = if (privateKey.publicKey == pub1) UInt8.zero else UInt8.one

    LnInvoiceSignature(recoverId = recoveryId, signature = sig)
  }

  /** The easiest way to create a [[org.bitcoins.core.protocol.ln.LnInvoice LnInvoice]]
    * is by just passing the given pareameters and
    * and then build will create a [[org.bitcoins.core.protocol.ln.LnInvoice LnInvoice]]
    * with a valid [[org.bitcoins.core.protocol.ln.LnInvoiceSignature LnInvoiceSignature]]
    * @param hrp the [[org.bitcoins.core.protocol.ln.LnHumanReadablePart LnHumanReadablePart]]
    * @param timestamp the timestamp on the invoice
    * @param lnTags the various tags in the invoice
    * @param privateKey - the key used to sign the invoice
    */
  def build(
      hrp: LnHumanReadablePart,
      timestamp: UInt64,
      lnTags: LnTaggedFields,
      privateKey: ECPrivateKey): LnInvoice = {
    val lnInvoiceSignature =
      buildLnInvoiceSignature(hrp, timestamp, lnTags, privateKey)

    LnInvoice(hrp = hrp,
              timestamp = timestamp,
              lnTags = lnTags,
              signature = lnInvoiceSignature)
  }

  /** The easiest way to create a [[org.bitcoins.core.protocol.ln.LnInvoice LnInvoice]]
    * is by just passing the given parameters and
    * and then build will create a [[org.bitcoins.core.protocol.ln.LnInvoice LnInvoice]]
    * with a valid [[org.bitcoins.core.protocol.ln.LnInvoiceSignature LnInvoiceSignature]]
    * @param hrp the [[org.bitcoins.core.protocol.ln.LnHumanReadablePart LnHumanReadablePart]]
    * @param lnTags the various tags in the invoice
    * @param privateKey - the key used to sign the invoice
    */
  def build(
      hrp: LnHumanReadablePart,
      lnTags: LnTaggedFields,
      privateKey: ECPrivateKey): LnInvoice = {
    val timestamp = UInt64(System.currentTimeMillis() / 1000L)
    val lnInvoiceSignature =
      buildLnInvoiceSignature(hrp, timestamp, lnTags, privateKey)

    LnInvoice(hrp = hrp,
              timestamp = timestamp,
              lnTags = lnTags,
              signature = lnInvoiceSignature)
  }

  private def uInt64ToBase32(input: UInt64): Vector[UInt5] = {
    var numNoPadding = LnUtil.encodeNumber(input.toBigInt)

    while (numNoPadding.length < 7) {
      numNoPadding = UInt5.zero +: numNoPadding
    }

    require(numNoPadding.length == 7)
    numNoPadding.toVector
  }

  /** Checks the checksum on a [[org.bitcoins.core.protocol.Bech32Address Bech32Address]]
    * and if it is valid, strips the checksum from @d and returns strictly
    * the payload
    * @param h - the [[org.bitcoins.core.protocol.ln.LnHumanReadablePart LnHumanReadablePart]] of the invoice
    * @param d - the payload WITH the checksum included
    */
  private def stripChecksumIfValid(
      h: LnHumanReadablePart,
      d: Vector[UInt5]): Try[Vector[UInt5]] = {
    if (verifyChecksum(h, d)) {
      if (d.size < 6) Success(Vector.empty)
      else Success(d.take(d.size - 6))
    } else
      Failure(
        new IllegalArgumentException("Checksum was invalid on the LnInvoice"))
  }
}
