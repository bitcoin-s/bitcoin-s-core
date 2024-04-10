package org.bitcoins.testkitcore.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.{core, crypto}
import org.bitcoins.crypto.{
  AesEncryptedData,
  AesIV,
  AesKey,
  AesPassword,
  CryptoUtil,
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECAdaptorSignature,
  ECDigitalSignature,
  ECPrivateKey,
  ECPublicKey,
  FieldElement,
  HashType,
  SchnorrDigitalSignature,
  SchnorrNonce,
  SchnorrPublicKey,
  Sha256Digest,
  Sha256DigestBE,
  Sha256Hash160Digest,
  SipHashKey,
  XOnlyPubKey
}
import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector}

/** Created by chris on 6/22/16.
  */
sealed abstract class CryptoGenerators {

  object entropy {

    /** Generates 128 bits of entropy
      */
    def bits128: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy128Bits)

    /** Generates 160 bits of entropy
      */
    def bits160: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy160Bits)

    /** Generates 192 bits of entropy
      */
    def bits192: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy192Bits)

    /** Generates 224 bits of entropy
      */
    def bits224: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy224Bits)

    /** Generates 256 bits of entropy
      */
    def bits256: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy256Bits)

    /** Generates either 128, 160, 192, 224 or 256 of bits of entropy
      */
    def any: Gen[BitVector] =
      Gen.oneOf(bits128, bits160, bits192, bits224, bits256)
  }

  def mnemonicCode128Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits128
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode160Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits160
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode192Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits192
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode224Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits224
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode256Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits256
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode: Gen[MnemonicCode] =
    Gen.oneOf(
      mnemonicCode128Bits,
      mnemonicCode160Bits,
      mnemonicCode192Bits,
      mnemonicCode224Bits,
      mnemonicCode256Bits
    )

  /** Generates a BIP39 valid mnemonic
    * phrase
    */
  def mnemonicPhrase: Gen[Vector[String]] =
    for {
      code <- mnemonicCode
    } yield code.words

  /** Generates a valid BIP39 seed from
    * an mnemonic with no password
    */
  def bip39SeedNoPassword: Gen[BIP39Seed] =
    for {
      code <- mnemonicCode
    } yield BIP39Seed.fromMnemonic(code)

  /** Generates a password that can be used with bip39
    * @see https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#From_mnemonic_to_seed
    */
  def bip39Password: Gen[String] = {
    Gen.alphaNumStr
  }

  /** Generates a valid BIP39 seed from
    * an mnemonic with a random password
    */
  def bip39SeedWithPassword: Gen[BIP39Seed] =
    for {
      code <- mnemonicCode
      pass <- bip39Password
    } yield BIP39Seed.fromMnemonic(code, pass)

  def privateKey: Gen[ECPrivateKey] = {
    //purposefully don't reach for cryptographically strong
    //number generation, we want determinism to reproduce failed
    //test cases. If we don't generate the private key with scalacheck
    //we won't be able to reproduce the test case with a seed
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/1339
    NumberGenerator.bytevector(32).map { vec =>
      ECPrivateKey.fromBytes(vec)
    }
  }

  def fieldElement: Gen[FieldElement] = privateKey.map(_.fieldElement)

  def smallFieldElement: Gen[FieldElement] =
    NumberGenerator
      .bytevector(30)
      .map(bytes => FieldElement(ByteVector.fill(2)(0) ++ bytes))

  def reallySmallFieldElement: Gen[FieldElement] =
    NumberGenerator
      .bytevector(15)
      .map(bytes => FieldElement(ByteVector.fill(17)(0) ++ bytes))

  def largeFieldElement: Gen[FieldElement] =
    NumberGenerator
      .bytevector(30)
      .map(bytes => FieldElement(ByteVector.fill(2)(Byte.MinValue) ++ bytes))

  def nonZeroFieldElement: Gen[FieldElement] =
    nonZeroPrivKey.map(_.fieldElement)

  /** Generates a random non-zero private key */
  def nonZeroPrivKey: Gen[ECPrivateKey] =
    privateKey.filter(_.bytes.toArray.exists(_ != 0.toByte))

  def schnorrNonce: Gen[SchnorrNonce] =
    nonZeroPrivKey.map(_.publicKey.bytes.tail).map(SchnorrNonce.fromBytes)

  def schnorrPublicKey: Gen[SchnorrPublicKey] =
    publicKey.map(_.schnorrPublicKey)

  def xOnlyPubKey: Gen[XOnlyPubKey] = publicKey.map(_.toXOnly)

  /** Generate a sequence of private keys
    * @param num maximum number of keys to generate
    * @return
    */
  def privateKeySeq(num: Int): Gen[Seq[ECPrivateKey]] =
    Gen.listOfN(num, privateKey)

  /** Generates a sequence of private keys, and determines an amount of 'required' private keys
    * that a transaction needs to be signed with
    * @param num the maximum number of keys to generate
    * @return
    */
  def privateKeySeqWithRequiredSigs(num: Int): Gen[(Seq[ECPrivateKey], Int)] = {
    if (num <= 0) {
      Gen.const((Nil, 0))
    } else {
      val privateKeys = privateKeySeq(num)
      for {
        keys <- privateKeys
        requiredSigs <- Gen.choose(0, keys.size - 1)
      } yield (keys, requiredSigs)
    }
  }

  /** Generates a random number of private keys less than 15.
    * Also generates a random 'requiredSigs' number that a transaction needs to be signed with
    */
  def privateKeySeqWithRequiredSigs: Gen[(Seq[ECPrivateKey], Int)] =
    for {
      num <- Gen.choose(0, 15)
      keysAndRequiredSigs <- privateKeySeqWithRequiredSigs(num)
    } yield keysAndRequiredSigs

  /** A generator with 7 or less private keys -- useful for creating smaller scripts */
  def smallPrivateKeySeqWithRequiredSigs: Gen[(Seq[ECPrivateKey], Int)] =
    for {
      num <- Gen.choose(0, 7)
      keysAndRequiredSigs <- privateKeySeqWithRequiredSigs(num)
    } yield keysAndRequiredSigs

  /** Generates a random public key */
  def publicKey: Gen[ECPublicKey] =
    for {
      privKey <- privateKey
    } yield privKey.publicKey

  /** Generates a random digital signature */
  def digitalSignature: Gen[ECDigitalSignature] =
    for {
      privKey <- privateKey
      hash <- CryptoGenerators.doubleSha256Digest
    } yield privKey.sign(hash)

  def digitalSignatureWithSigHash: Gen[ECDigitalSignature] = {
    for {
      sig <- digitalSignature
      sigHash <- hashType
    } yield {
      ECDigitalSignature(sig.bytes :+ sigHash.byte)
    }
  }

  def schnorrDigitalSignature: Gen[SchnorrDigitalSignature] = {
    for {
      privKey <- privateKey
      hash <- CryptoGenerators.doubleSha256Digest
    } yield privKey.schnorrSign(hash.bytes)
  }

  def adaptorSignature: Gen[ECAdaptorSignature] = {
    for {
      tweakedNonce <- publicKey
      untweakedNonce <- publicKey
      adaptedS <- fieldElement
      proofE <- fieldElement
      proofS <- fieldElement
    } yield {
      ECAdaptorSignature(tweakedNonce, untweakedNonce, adaptedS, proofE, proofS)
    }
  }

  def sha256Digest: Gen[Sha256Digest] =
    for {
      bytes <- NumberGenerator.bytevector
      digest = CryptoUtil.sha256(bytes)
    } yield digest

  def sha256DigestBE: Gen[Sha256DigestBE] = {
    sha256Digest.map(_.flip)
  }

  /** Generates a random [[DoubleSha256Digest DoubleSha256Digest]] */
  def doubleSha256Digest: Gen[DoubleSha256Digest] =
    for {
      key <- privateKey
      digest = CryptoUtil.doubleSHA256(key.bytes)
    } yield digest

  def doubleSha256DigestBE: Gen[DoubleSha256DigestBE] = {
    doubleSha256Digest.map(_.flip)
  }

  /** Generates a sequence of [[DoubleSha256Digest DoubleSha256Digest]]
    * @param num the number of digets to generate
    * @return
    */
  def doubleSha256DigestSeq(num: Int): Gen[Seq[DoubleSha256Digest]] =
    Gen.listOfN(num, doubleSha256Digest)

  /** Generates a random [[Sha256Hash160Digest Sha256Hash160Digest]] */
  def sha256Hash160Digest: Gen[Sha256Hash160Digest] =
    for {
      pubKey <- publicKey
      hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    } yield hash

  /** Generates a random [[HashType HashType]] */
  def hashType: Gen[HashType] = Gen.oneOf(HashType.hashTypes)

  def extVersion: Gen[ExtKeyVersion] = {
    Gen.oneOf(ExtKeyVersion.all)
  }

  /** Generates an [[ExtPrivateKey ExtPrivateKey]] */
  def extPrivateKey: Gen[ExtPrivateKey] = {
    for {
      version <- Gen.oneOf(ExtKeyVersion.allPrivs)
      ext = core.crypto.ExtPrivateKey(version)
    } yield ext
  }

  def extPublicKey: Gen[ExtPublicKey] = extPrivateKey.map(_.extPublicKey)

  def extKey: Gen[ExtKey] = Gen.oneOf(extPrivateKey, extPublicKey)

  def aesKey128Bit: Gen[AesKey] = Gen.delay(AesKey.get128Bit())
  def aesKey192Bit: Gen[AesKey] = Gen.delay(AesKey.get192Bit())
  def aesKey256Bit: Gen[AesKey] = Gen.delay(AesKey.get256Bit())

  def aesKey: Gen[AesKey] =
    Gen.oneOf(aesKey128Bit, aesKey192Bit, aesKey256Bit)

  def aesPassword: Gen[AesPassword] =
    Gen.alphaStr.suchThat(_.nonEmpty).map(AesPassword.fromNonEmptyString(_))

  def aesIV: Gen[AesIV] = Gen.delay(AesIV.random)

  def aesEncryptedData: Gen[AesEncryptedData] =
    for {
      cipher <- NumberGenerator.bytevector.suchThat(_.nonEmpty)
      iv <- aesIV
    } yield crypto.AesEncryptedData(cipherText = cipher, iv)

  def genKey: Gen[SipHashKey] =
    Gen
      .listOfN(16, NumberGenerator.byte)
      .map(ByteVector(_))
      .map(SipHashKey(_))

  def genPMRand: Gen[(UInt8, UInt64, UInt64)] =
    NumberGenerator.genP.flatMap { p =>
      // If hash's quotient when divided by 2^p is too large, we hang converting to unary
      val upperBound: Long = p.toInt * 1000 + 1

      val mGen = Gen
        .chooseNum(1L, upperBound)
        .map(UInt64(_))

      mGen.flatMap { m =>
        val upperBound = (m.toInt * 2 - 2).toLong

        val randGen = Gen.chooseNum(0L, upperBound).map(UInt64(_))

        randGen.map(rand => (p, m, rand))
      }
    }

}

object CryptoGenerators extends CryptoGenerators
