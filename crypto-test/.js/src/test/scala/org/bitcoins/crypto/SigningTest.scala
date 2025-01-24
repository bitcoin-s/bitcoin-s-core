package org.bitcoins.crypto

import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

class SigningTest extends BitcoinSCryptoTest {

  it must "be able to sign and verify signatures" in {
    val privkey = ECPrivateKey.freshPrivateKey
    val pubkey = privkey.publicKey
    val msg = BCryptoCryptoRuntime.randomBytes(32)
    val sig = privkey.sign(msg)
    assert(pubkey.verify(msg, sig))
  }

  it must "be able to sign and verify Schnorr signatures" in {
    val privkey = ECPrivateKey.freshPrivateKey
    val pubkey = privkey.publicKey
    val msg = BCryptoCryptoRuntime.randomBytes(32)
    val sig = privkey.schnorrSign(msg)
    assert(pubkey.schnorrVerify(msg, sig))
  }

  it must "pass the BIP 340 test-vectors with bcrypto" in {
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

  def testSign(
      index: Int,
      secKey: ECPrivateKey,
      auxRand: ByteVector,
      msg: ByteVector,
      expectedSig: SchnorrDigitalSignature
  ): Assertion = {
    val bcryptoSig =
      BCryptoCryptoRuntime.schnorrSign(msg, secKey, auxRand)
    assert(
      bcryptoSig == expectedSig,
      s"Test $index failed signing for Bouncy Castle"
    )
  }

  def testVerify(
      index: Int,
      pubKey: SchnorrPublicKey,
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      expectedResult: Boolean,
      comment: String
  ): Assertion = {
    val bcryptoResult =
      BCryptoCryptoRuntime.schnorrVerify(msg, pubKey, sig)
    assert(
      bcryptoResult == expectedResult,
      s"Test $index failed verification for Bouncy Castle: $comment"
    )
  }

}
