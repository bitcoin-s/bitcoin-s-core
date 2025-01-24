package org.bitcoins.crypto

import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/** Tests from
  * https://github.com/sipa/bips/blob/bip-taproot/bip-0340/test-vectors.csv
  */
class BouncyCastleBIP340Test extends BitcoinSCryptoTest {
  behavior of "Schnorr Signing"

  def testSign(
      index: Int,
      secKey: ECPrivateKey,
      auxRand: ByteVector,
      msg: ByteVector,
      expectedSig: SchnorrDigitalSignature
  ): Assertion = {
    val secpSig = secKey.schnorrSign(msg, auxRand)
    val bouncyCastleSig =
      BouncycastleCryptoRuntime.schnorrSign(msg, secKey, auxRand)
    assert(
      secpSig == expectedSig,
      s"Test $index failed signing for libsecp256k1"
    )
    assert(
      bouncyCastleSig == expectedSig,
      s"Test $index failed signing for Bouncy Castle"
    )
    assert(bouncyCastleSig == secpSig)
  }

  def testVerify(
      index: Int,
      pubKey: SchnorrPublicKey,
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      expectedResult: Boolean,
      comment: String
  ): Assertion = {
    val secpResult = Try(pubKey.verify(msg, sig)).getOrElse(false)
    val bouncyCastleResult =
      Try(BouncycastleCryptoRuntime.schnorrVerify(msg, pubKey, sig))
        .getOrElse(false)
    assert(
      secpResult == expectedResult,
      s"Test $index failed verification for libsecp256k1: $comment"
    )
    assert(
      bouncyCastleResult == expectedResult,
      s"Test $index failed verification for Bouncy Castle: $comment"
    )
    assert(bouncyCastleResult == secpResult)
  }

  def test(
      index: Int,
      secKeyOpt: Option[String],
      pubKey: String,
      auxRandOpt: Option[String],
      msg: String,
      sig: String,
      result: Boolean,
      comment: String
  ): Assertion = {
    val pkT = Try(SchnorrPublicKey(pubKey))
    val msgBytes = ByteVector.fromHex(msg).get
    val schnorrSigT = Try(SchnorrDigitalSignature(sig))

    (pkT, schnorrSigT) match {
      case (Success(pk), Success(schnorrSig)) =>
        (secKeyOpt, auxRandOpt) match {
          case (Some(secKeyStr), Some(auxRandStr)) =>
            val secKey = ECPrivateKey(secKeyStr)
            assert(secKey.schnorrPublicKey == pk)
            val auxRand = ByteVector.fromHex(auxRandStr).get
            testSign(index, secKey, auxRand, msgBytes, schnorrSig)
          case _ => ()
        }

        testVerify(index, pk, msgBytes, schnorrSig, result, comment)
      case (Failure(_), _) | (_, Failure(_)) => // Must be verify only test resulting in false
        assert(secKeyOpt.isEmpty)
        assert(auxRandOpt.isEmpty)
        assert(!result, s"Test $index failed to parse signature: $comment")
    }
  }

  it must "pass the BIP 340 test-vectors with both secp256k1 bindings and bouncy castle" in {
    BIP340TestVectors.vectors.foreach {
      case (index, secKeyOpt, pubKey, auxRandOpt, msg, sig, result, comment) =>
        test(
          index = index,
          secKeyOpt = secKeyOpt,
          pubKey = pubKey,
          auxRandOpt = auxRandOpt,
          msg = msg,
          sig = sig,
          result = result,
          comment = comment
        )
    }
  }
}
