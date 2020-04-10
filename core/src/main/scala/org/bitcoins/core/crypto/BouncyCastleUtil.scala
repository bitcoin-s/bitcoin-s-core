package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil}
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.{
  ECPrivateKeyParameters,
  ECPublicKeyParameters
}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.{ECCurve, ECPoint}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

object BouncyCastleUtil {

  private val curve: ECCurve = CryptoParams.curve.getCurve
  private val G: ECPoint = CryptoParams.curve.getG
  private val N: BigInteger = CryptoParams.curve.getN

  private def getBigInteger(bytes: ByteVector): BigInteger = {
    new BigInteger(1, bytes.toArray)
  }

  def addNumbers(num1: ByteVector, num2: ByteVector): BigInteger = {
    val bigInteger1 = getBigInteger(num1)
    val bigInteger2 = getBigInteger(num2)

    bigInteger1.add(bigInteger2).mod(N)
  }

  def multiplyNumbers(num1: ByteVector, num2: ByteVector): BigInteger = {
    val bigInteger1 = getBigInteger(num1)
    val bigInteger2 = getBigInteger(num2)

    bigInteger1.multiply(bigInteger2).mod(N)
  }

  def pubKeyTweakMul(publicKey: ECPublicKey, tweak: ByteVector): ECPublicKey = {
    val point = publicKey.toPoint.multiply(getBigInteger(tweak))
    ECPublicKey.fromPoint(point, publicKey.isCompressed)
  }

  def decodePoint(bytes: ByteVector): ECPoint = {
    curve.decodePoint(bytes.toArray)
  }

  def validatePublicKey(bytes: ByteVector): Boolean = {
    Try(decodePoint(bytes))
      .map(_.getCurve == curve)
      .getOrElse(false)
  }

  def decompressPublicKey(publicKey: ECPublicKey): ECPublicKey = {
    if (publicKey.isCompressed) {
      val point = decodePoint(publicKey.bytes)
      val decompressedBytes =
        ByteVector.fromHex("04").get ++
          ByteVector(point.getXCoord.getEncoded) ++
          ByteVector(point.getYCoord.getEncoded)
      ECPublicKey(decompressedBytes)
    } else publicKey
  }

  def computePublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val priv = getBigInteger(privateKey.bytes)
    val point = G.multiply(priv)
    val pubBytes = ByteVector(point.getEncoded(privateKey.isCompressed))
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"Bouncy Castle failed to generate a valid public key, got: ${BitcoinSUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  def sign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(
      new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters =
      new ECPrivateKeyParameters(getBigInteger(privateKey.bytes),
                                 CryptoParams.curve)
    signer.init(true, privKey)
    val components: Array[BigInteger] =
      signer.generateSignature(dataToSign.toArray)
    val (r, s) = (components(0), components(1))
    val signature = ECDigitalSignature(r, s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(
      signatureLowS.isDEREncoded,
      "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }

  def verifyDigitalSignature(
      data: ByteVector,
      publicKey: ECPublicKey,
      signature: ECDigitalSignature): Boolean = {
    val resultTry = Try {
      val publicKeyParams =
        new ECPublicKeyParameters(decodePoint(publicKey.bytes),
                                  CryptoParams.curve)

      val signer = new ECDSASigner
      signer.init(false, publicKeyParams)
      signature match {
        case EmptyDigitalSignature =>
          signer.verifySignature(data.toArray,
                                 java.math.BigInteger.valueOf(0),
                                 java.math.BigInteger.valueOf(0))
        case _: ECDigitalSignature =>
          val rBigInteger: BigInteger = new BigInteger(signature.r.toString())
          val sBigInteger: BigInteger = new BigInteger(signature.s.toString())
          signer.verifySignature(data.toArray, rBigInteger, sBigInteger)
      }
    }
    resultTry.getOrElse(false)
  }

  def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    val nonceKey =
      SchnorrNonce.kFromBipSchnorr(privateKey, dataToSign, auxRand)
    val rx = nonceKey.schnorrNonce

    val k = nonceKey.bytes
    val x = privateKey.schnorrKey.bytes
    val e = CryptoUtil
      .taggedSha256(rx.bytes ++ privateKey.schnorrPublicKey.bytes ++ dataToSign,
                    "BIP340/challenge")
      .bytes

    val challenge = ByteVector(multiplyNumbers(e, x).toByteArray)
    val sig = addNumbers(k, challenge)

    SchnorrDigitalSignature(rx, ByteVector(sig.toByteArray).takeRight(32))
  }

  def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean = {
    val rx = signature.rx
    val sT = Try(ECPrivateKey(signature.sig))

    sT match {
      case Success(s) =>
        val e = CryptoUtil
          .taggedSha256(rx.bytes ++ schnorrPubKey.bytes ++ data,
                        "BIP340/challenge")
          .bytes
        val negE = ECPrivateKey(e).negate.bytes

        val sigPoint = s.publicKey
        val challengePointT = Try(pubKeyTweakMul(schnorrPubKey.publicKey, negE))

        val resultT = challengePointT.map { challengePoint =>
          val computedR = challengePoint.add(sigPoint)
          computedR.toPoint.getRawYCoord.sqrt() != null &&
          computedR.schnorrNonce == rx
        }

        resultT.getOrElse(false)
      case Failure(_) => false
    }
  }

  def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey = {
    val e = CryptoUtil
      .taggedSha256(nonce.bytes ++ pubKey.bytes ++ data, "BIP340/challenge")
      .bytes

    val compressedSigPoint =
      nonce.publicKey.add(pubKeyTweakMul(pubKey.publicKey, e))

    if (compressed) {
      compressedSigPoint
    } else {
      compressedSigPoint.decompressed
    }
  }
}
