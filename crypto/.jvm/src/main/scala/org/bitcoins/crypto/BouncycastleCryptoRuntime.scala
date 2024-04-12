package org.bitcoins.crypto

import org.bouncycastle.crypto.digests.{
  RIPEMD160Digest,
  SHA256Digest,
  SHA512Digest
}
import org.bouncycastle.crypto.macs.{HMac, SipHash}
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.math.ec.{ECPoint, WNafUtil}
import org.bouncycastle.util.BigIntegers
import scodec.bits.ByteVector

import java.math.BigInteger
import java.security.{MessageDigest, SecureRandom}

/** This is an implementation of [[CryptoRuntime]] that defaults to Bouncy Castle (https://bouncycastle.org/)
  * and [[java.security]].
  */
trait BouncycastleCryptoRuntime extends CryptoRuntime {
  private lazy val secureRandom = new SecureRandom()

  override val cryptoContext: CryptoContext = CryptoContext.BouncyCastle

  /** Cribbed from ECKeyPairGenerator::generateKeyPair
    * @see https://github.com/bcgit/bc-java/blob/63b18eb973f5731e403f655ee81d6b8456f5b256/core/src/main/java/org/bouncycastle/crypto/generators/ECKeyPairGenerator.java#L39
    */
  override def freshPrivateKey: ECPrivateKey = {
    val n = CryptoParams.getN
    val bitLength = n.bitLength()
    val minWeight = bitLength >>> 2

    var priv: BigInteger = BigInteger.ZERO
    var foundNum: Boolean = false
    while (!foundNum) {
      priv = BigIntegers.createRandomBigInteger(bitLength, secureRandom)

      if (priv.compareTo(BigInteger.ONE) < 0 || (priv.compareTo(n) >= 0)) {
        () //do nothing, keep iterating
      } else if (WNafUtil.getNafWeight(priv) < minWeight) {
        () //do nothing, keep iterating
      } else {
        foundNum = true
      }
    }
    val bytes = ByteVector(priv.toByteArray)
    ECPrivateKey.fromBytes(bytes.padLeft(33))
  }

  /** @param x x coordinate
    * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
    *         p1.y is even, p2.y is odd
    */
  def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
    val bytes = ByteVector(x.toByteArray)

    val bytes32 = if (bytes.length < 32) {
      bytes.padLeft(32)
    } else if (bytes.length == 32) {
      bytes
    } else if (bytes.length == 33 && bytes.head == 0.toByte) {
      bytes.tail
    } else {
      throw new IllegalArgumentException(
        s"Field element cannot have more than 32 bytes, got $bytes from $x")
    }

    (BouncyCastleUtil.decodePoint(ECPublicKey(0x02.toByte +: bytes32)),
     BouncyCastleUtil.decodePoint(ECPublicKey(0x03.toByte +: bytes32)))
  }

  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {

    val curve = BouncyCastleCryptoParams.curve
    val (r, s) = (signature.r.bigInteger, signature.s.bigInteger)

    val m = new BigInteger(1, message.toArray)

    val (p1, p2) = recoverPoint(r)

    val Q1 = p1
      .multiply(s)
      .subtract(curve.getG.multiply(m))
      .multiply(r.modInverse(curve.getN))
    val Q2 = p2
      .multiply(s)
      .subtract(curve.getG.multiply(m))
      .multiply(r.modInverse(curve.getN))

    val pub1 = BouncyCastleUtil.decodePubKey(Q1)
    val pub2 = BouncyCastleUtil.decodePubKey(Q2)
    (pub1, pub2)
  }

  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val hmac512 = new HMac(new SHA512Digest())
    hmac512.init(new KeyParameter(key.toArray))
    hmac512.update(data.toArray, 0, data.intSize.get)
    val output = new Array[Byte](64)
    hmac512.doFinal(output, 0)
    ByteVector(output)
  }

  override def hmac256(key: ByteVector, data: ByteVector): ByteVector = {
    val mac = new HMac(new SHA256Digest())
    mac.init(new KeyParameter(key.toArray))
    mac.update(data.toArray, 0, data.length.toInt)
    val output = new Array[Byte](32)
    mac.doFinal(output, 0)
    ByteVector.view(output)
  }

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    //from this tutorial http://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    val raw = bytes.toArray
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    RipeMd160Digest(ByteVector(out))
  }

  override def sha256(bytes: ByteVector): Sha256Digest = {
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
    Sha256Digest(ByteVector(hash))
  }

  override def sha3_256(bytes: ByteVector): Sha3_256Digest = {
    val hash = MessageDigest.getInstance("SHA3-256").digest(bytes.toArray)
    Sha3_256Digest(ByteVector(hash))
  }

  override def sha1(bytes: ByteVector): Sha1Digest = {
    val hash = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList
    Sha1Digest(ByteVector(hash))
  }

  override def normalize(str: String): String = {
    java.text.Normalizer.normalize(str, java.text.Normalizer.Form.NFC)
  }

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val hash = ripeMd160(sha256(bytes).bytes).bytes
    Sha256Hash160Digest(hash)
  }

  override def toPublicKey(privateKey: ECPrivateKeyBytes): ECPublicKey = {
    BouncyCastleUtil.computePublicKey(privateKey)
  }

  override def sign(
      privateKey: ECPrivateKey,
      dataToSign: ByteVector): ECDigitalSignature = {
    BouncyCastleUtil.sign(dataToSign, privateKey)
  }

  override def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature =
    BouncyCastleUtil.signWithEntropy(bytes, privateKey, entropy)

  override def secKeyVerify(privateKeyBytes: ByteVector): Boolean = {
    val num = new BigInteger(1, privateKeyBytes.toArray)

    BouncyCastleCryptoParams.curve.getCurve.isValidFieldElement(num) &&
    num.compareTo(BouncyCastleCryptoParams.curve.getN) < 0 &&
    num != BigInteger.ZERO
  }

  override def verify(
      publicKey: ECPublicKeyApi,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean =
    BouncyCastleUtil.verifyDigitalSignature(data, publicKey, signature)

  override def publicKey(privateKey: ECPrivateKeyBytes): ECPublicKey =
    BouncyCastleUtil.computePublicKey(privateKey)

  override def tweakMultiply(
      publicKey: ECPublicKey,
      tweak: FieldElement): ECPublicKey =
    BouncyCastleUtil.pubKeyTweakMul(publicKey, tweak.bytes)

  override def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey = {
    val p1 = BouncyCastleUtil.decodePoint(pk1)
    val p2 = BouncyCastleUtil.decodePoint(pk2)
    val sumPoint = p1.add(p2)
    BouncyCastleUtil.decodePubKey(sumPoint)
  }

  def pubKeyTweakAdd(
      pubkey: ECPublicKey,
      privkey: ECPrivateKey): ECPublicKey = {
    val tweak = privkey.publicKey
    pubkey.add(tweak)
  }

  override def sipHash(item: ByteVector, key: SipHashKey): Long = {
    val sipHashCParam = 2
    val sipHashDParam = 4

    val sh = new SipHash(sipHashCParam, sipHashDParam)

    val keyParam = new KeyParameter(key.bytes.toArray)

    sh.init(keyParam)

    val offset = 0

    sh.update(item.toArray, offset, item.length.toInt)

    sh.doFinal()
  }

  override def decodePoint(bytes: ByteVector): SecpPoint = {
    val decoded = BouncyCastleUtil.decodePoint(bytes)

    if (decoded.isInfinity)
      SecpPointInfinity
    else
      SecpPoint(decoded.getRawXCoord.getEncoded,
                decoded.getRawYCoord.getEncoded)
  }

  override def pbkdf2WithSha512(
      pass: ByteVector,
      salt: ByteVector,
      iterationCount: Int,
      derivedKeyLength: Int): ByteVector = {
    val bytes =
      PBKDF2.withSha512(pass, salt, iterationCount, derivedKeyLength).getEncoded
    ByteVector(bytes)
  }

  override def randomBytes(n: Int): ByteVector = {
    val array = new Array[Byte](n)
    secureRandom.nextBytes(array)
    ByteVector(array)
  }
}

object BouncycastleCryptoRuntime extends BouncycastleCryptoRuntime
