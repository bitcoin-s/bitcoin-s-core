package org.bitcoins.crypto

import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

import java.math.BigInteger

/** Utility cryptographic functions
  * This is a proxy for the underlying implementation of [[CryptoRuntime]]
  * such as [[LibSecp256k1CryptoRuntime]].
  *
  * This is necessary so that the core module doesn't need to be refactored
  * to add support for multiple platforms, it can keep referencing CryptoUtil
  */
trait CryptoUtil extends CryptoRuntime {

  /** The underlying bitcoin-s-crypto-runtime-factory for the specific platform we are running on */
  private lazy val cryptoRuntime: CryptoRuntime = CryptoContext.cryptoRuntime

  override lazy val cryptoContext: CryptoContext = cryptoRuntime.cryptoContext

  override def freshPrivateKey: ECPrivateKey = {
    cryptoRuntime.freshPrivateKey
  }

  override def toPublicKey(
      privateKey: ECPrivateKey,
      isCompressed: Boolean): ECPublicKey = {
    cryptoRuntime.toPublicKey(privateKey, isCompressed)
  }

  override def normalize(str: String): String = {
    cryptoRuntime.normalize(str)
  }

  /** Does the following computation: RIPEMD160(SHA256(hex)). */
  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    cryptoRuntime.sha256Hash160(bytes)
  }

  def sha256Hash160(str: String): Sha256Hash160Digest = {
    cryptoRuntime.sha256Hash160(serializeForHash(str))
  }

  def doubleSHA256(str: String): DoubleSha256Digest = {
    doubleSHA256(serializeForHash(str))
  }

  /** Takes sha256(bytes). */
  override def sha256(bytes: ByteVector): Sha256Digest = {
    cryptoRuntime.sha256(bytes)
  }

  def taggedSha256(str: String, tag: String): Sha256Digest = {
    taggedSha256(serializeForHash(str), tag)
  }

  /** Performs SHA1(bytes). */
  override def sha1(bytes: ByteVector): Sha1Digest = {
    cryptoRuntime.sha1(bytes)
  }

  def sha1(str: String): Sha1Digest = {
    sha1(serializeForHash(str))
  }

  /** Performs RIPEMD160(bytes). */
  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    cryptoRuntime.ripeMd160(bytes)
  }

  def ripeMd160(str: String): RipeMd160Digest = {
    ripeMd160(serializeForHash(str))
  }

  /** Calculates `HMAC-SHA512(key, data)`
    */
  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    cryptoRuntime.hmac512(key, data)
  }

  /** @param x x coordinate
    * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
    *         p1.y is even, p2.y is odd
    */
  def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
    cryptoRuntime.recoverPoint(x)
  }

  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {
    cryptoRuntime.recoverPublicKey(signature, message)
  }

  override def publicKey(privateKey: ECPrivateKey): ECPublicKey =
    cryptoRuntime.publicKey(privateKey)

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature =
    cryptoRuntime.sign(privateKey, dataToSign)

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature =
    cryptoRuntime.signWithEntropy(privateKey, bytes, entropy)

  override def secKeyVerify(privateKeybytes: ByteVector): Boolean =
    cryptoRuntime.secKeyVerify(privateKeybytes)

  override def verify(
      publicKey: ECPublicKey,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean =
    cryptoRuntime.verify(publicKey, data, signature)

  override def decompressed(publicKey: ECPublicKey): ECPublicKey =
    cryptoRuntime.decompressed(publicKey)

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey =
    cryptoRuntime.tweakMultiply(publicKey, tweak)

  override def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey =
    cryptoRuntime.add(pk1, pk2)

  override def add(pk1: ByteVector, pk2: ECPrivateKey): ByteVector =
    cryptoRuntime.add(pk1, pk2)

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey =
    cryptoRuntime.add(pk1, pk2)

  override def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey =
    cryptoRuntime.pubKeyTweakAdd(pubkey, privkey)

  override def isValidPubKey(bytes: ByteVector): Boolean =
    cryptoRuntime.isValidPubKey(bytes)

  override def isFullyValidWithBouncyCastle(bytes: ByteVector): Boolean =
    cryptoRuntime.isFullyValidWithBouncyCastle(bytes)

  override def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature =
    cryptoRuntime.schnorrSign(dataToSign, privateKey, auxRand)

  override def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature =
    cryptoRuntime.schnorrSignWithNonce(dataToSign, privateKey, nonceKey)

  override def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean =
    cryptoRuntime.schnorrVerify(data, schnorrPubKey, signature)

  override def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey =
    cryptoRuntime.schnorrComputeSigPoint(data, nonce, pubKey, compressed)
}

object CryptoUtil extends CryptoUtil
