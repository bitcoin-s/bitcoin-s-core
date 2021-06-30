package org.bitcoins.crypto

import scodec.bits.{BitVector, ByteVector}

import scala.util.{Failure, Success, Try}

/** Trait that should be extended by specific runtimes like javascript
  * or the JVM to support crypto functions needed for bitcoin-s
  */
trait CryptoRuntime {

  def cryptoContext: CryptoContext

  /** Generates a 32 byte private key */
  def freshPrivateKey: ECPrivateKey

  /** Converts a private key -> public key
    * @param privateKey the private key we want the corresponding public key for
    * @param isCompressed whether the returned public key should be compressed or not
    */
  def toPublicKey(privateKey: ECPrivateKeyBytes): ECPublicKey

  def ripeMd160(bytes: ByteVector): RipeMd160Digest

  def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest

  def sha256(bytes: ByteVector): Sha256Digest

  def sha256(str: String): Sha256Digest = {
    sha256(serializeForHash(str))
  }

  def sha256(bitVector: BitVector): Sha256Digest = {
    sha256(bitVector.toByteVector)
  }

  def sha3_256(bytes: ByteVector): Sha3_256Digest

  def taggedSha256(bytes: ByteVector, tag: String): Sha256Digest = {
    val tagHash = sha256(tag)
    val tagBytes = tagHash.bytes ++ tagHash.bytes
    sha256(tagBytes ++ bytes)
  }

  /** Performs sha256(sha256(bytes)). */
  def doubleSHA256(bytes: ByteVector): DoubleSha256Digest = {
    val hash: ByteVector = sha256(sha256(bytes).bytes).bytes
    DoubleSha256Digest(hash)
  }

  def sha1(bytes: ByteVector): Sha1Digest

  def hmac512(key: ByteVector, data: ByteVector): ByteVector

  def hmac256(key: ByteVector, data: ByteVector): ByteVector

  def normalize(str: String): String

  def serializeForHash(str: String): ByteVector = {
    ByteVector(normalize(str).getBytes("UTF-8"))
  }

  // The tag "BIP0340/challenge"
  private lazy val schnorrChallengeTagBytes = {
    ByteVector
      .fromValidHex(
        "7bb52d7a9fef58323eb1bf7a407db382d2f3f2d81bb1224f49fe518f6d48d37c7bb52d7a9fef58323eb1bf7a407db382d2f3f2d81bb1224f49fe518f6d48d37c"
      )
  }

  def sha256SchnorrChallenge(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrChallengeTagBytes ++ bytes)
  }

  // The tag "DLEQ"
  private lazy val dleqTagBytes = {
    ByteVector
      .fromValidHex(
        "4ba8f2d94f2cbc10e555e8befa96b62f14cd7396aff9cdaf0f638c673166d5274ba8f2d94f2cbc10e555e8befa96b62f14cd7396aff9cdaf0f638c673166d527"
      )
  }

  def sha256DLEQ(bytes: ByteVector): Sha256Digest = {
    sha256(dleqTagBytes ++ bytes)
  }

  // The tag "ECDSAadaptor/aux"
  private lazy val ecdsaAdaptorAuxTagBytes = {
    ByteVector
      .fromValidHex(
        "bffb017fd37ef53b393ddd2e2a168c340b3ee5dd4e53c89de63a74b1d9061b0dbffb017fd37ef53b393ddd2e2a168c340b3ee5dd4e53c89de63a74b1d9061b0d"
      )
  }

  def sha256ECDSAAdaptorAux(bytes: ByteVector): Sha256Digest = {
    sha256(ecdsaAdaptorAuxTagBytes ++ bytes)
  }

  // The tag "ECDSAadaptor/non"
  private lazy val ecdsaAdaptorNonceTagBytes = {
    ByteVector
      .fromValidHex(
        "848f232fbe7f662cf520de8e335986eaf63a6617dd2a8b28f3a180a681d6f161848f232fbe7f662cf520de8e335986eaf63a6617dd2a8b28f3a180a681d6f161"
      )
  }

  def sha256ECDSAAdaptorNonce(bytes: ByteVector): Sha256Digest = {
    sha256(ecdsaAdaptorNonceTagBytes ++ bytes)
  }

  // The tag "DLC/oracle/attestation/v0"
  private val dlcAttestationTagBytes = {
    ByteVector
      .fromValidHex(
        "0c2fa46216e6e460e5e3f78555b102c5ac6aecabbfb82b430cf36cdfe04421790c2fa46216e6e460e5e3f78555b102c5ac6aecabbfb82b430cf36cdfe0442179"
      )
  }

  def sha256DLCAttestation(bytes: ByteVector): Sha256Digest = {
    sha256(dlcAttestationTagBytes ++ bytes)
  }

  def sha256DLCAttestation(str: String): Sha256Digest = {
    sha256DLCAttestation(serializeForHash(str))
  }

  // The tag "DLC/oracle/announcement/v0"
  private val dlcAnnouncementTagBytes = {
    ByteVector
      .fromValidHex(
        "6378871e8c99d480fff016e178a371e7e058445eff3023fe158f05aa185ed0e16378871e8c99d480fff016e178a371e7e058445eff3023fe158f05aa185ed0e1"
      )
  }

  def sha256DLCAnnouncement(bytes: ByteVector): Sha256Digest = {
    sha256(dlcAnnouncementTagBytes ++ bytes)
  }

  /** Recover public keys from a signature and the message that was signed. This method will return 2 public keys, and the signature
    * can be verified with both, but only one of them matches that private key that was used to generate the signature.
    *
    * @param signature       signature
    * @param message message that was signed
    * @return a (pub1, pub2) tuple where pub1 and pub2 are candidates public keys. If you have the recovery id  then use
    *         pub1 if the recovery id is even and pub2 if it is odd
    */
  def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey)

  // The tag "BIP0340/aux"
  private val schnorrAuxTagBytes = {
    ByteVector
      .fromValidHex(
        "f1ef4e5ec063cada6d94cafa9d987ea069265839ecc11f972d77a52ed8c1cc90f1ef4e5ec063cada6d94cafa9d987ea069265839ecc11f972d77a52ed8c1cc90"
      )
  }

  def sha256SchnorrAuxRand(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrAuxTagBytes ++ bytes)
  }

  // The tag "BIP0340/nonce"
  private val schnorrNonceTagBytes = {
    ByteVector
      .fromValidHex(
        "07497734a79bcb355b9b8c7d034f121cf434d73ef72dda19870061fb52bfeb2f07497734a79bcb355b9b8c7d034f121cf434d73ef72dda19870061fb52bfeb2f"
      )
  }

  def sha256SchnorrNonce(bytes: ByteVector): Sha256Digest = {
    sha256(schnorrNonceTagBytes ++ bytes)
  }

  def publicKey(privateKey: ECPrivateKeyBytes): ECPublicKey

  def sign(privateKey: ECPrivateKey, dataToSign: ByteVector): ECDigitalSignature

  def signWithEntropy(
      privateKey: ECPrivateKey,
      bytes: ByteVector,
      entropy: ByteVector): ECDigitalSignature

  def secKeyVerify(privateKeybytes: ByteVector): Boolean

  def verify(
      publicKey: ECPublicKeyApi,
      data: ByteVector,
      signature: ECDigitalSignature): Boolean

  def decompressed(pubKeyBytes: ByteVector): ByteVector = {
    decodePoint(pubKeyBytes) match {
      case SecpPointInfinity => ByteVector(0x00)
      case point: SecpPointFinite =>
        ByteVector.fromHex("04").get ++
          point.x.bytes ++
          point.y.bytes
    }
  }

  def decompressed[PK <: ECPublicKeyApi](
      pubKeyBytes: ByteVector,
      fromBytes: ByteVector => PK): PK = {
    fromBytes(decompressed(pubKeyBytes))
  }

  def decompressed[PK <: ECPublicKeyApi](publicKey: PK): publicKey.type = {
    if (publicKey.isDecompressed) publicKey
    else decompressed(publicKey.bytes, publicKey.fromBytes(_))
  }

  def tweakMultiply(publicKey: ECPublicKey, tweak: FieldElement): ECPublicKey

  def tweakMultiply(point: SecpPoint, tweak: FieldElement): SecpPoint = {
    point match {
      case SecpPointInfinity => SecpPointInfinity
      case pt: SecpPointFinite =>
        val pub = pt.toPublicKey
        Try(tweakMultiply(pub, tweak)) match {
          case Failure(err) =>
            /* If tweak*pub = 0, then (tweak - 1)*pub + pub = 0
             * so (tweak - 1)*pub = -pub. Otherwise err is unrelated.
             * (Noting that it is impossible to have
             * tweak*pub = 0 = (tweak - 1)*pub for pub != 0)
             */
            val pubNeg = tweakMultiply(pub, tweak.subtract(FieldElement.one))
            if (pubNeg == pub.negate) SecpPointInfinity
            else throw err
          case Success(multKey) => multKey.toPoint
        }
    }
  }

  def add(pk1: ECPrivateKey, pk2: ECPrivateKey): ECPrivateKey =
    pk1.fieldElement.add(pk2.fieldElement).toPrivateKey

  def add(pk1: ByteVector, pk2: ECPrivateKey): ByteVector = {
    val sum = pk2.fieldElement.add(FieldElement(pk1))
    sum.bytes
  }

  /** Adds two SecpPoints together and correctly handles the point at infinity (0x00). */
  def add(point1: SecpPoint, point2: SecpPoint): SecpPoint = {
    (point1, point2) match {
      case (SecpPointInfinity, p) => p
      case (p, SecpPointInfinity) => p
      case (p1: SecpPointFinite, p2: SecpPointFinite) =>
        val pk1 = p1.toPublicKey
        val pk2 = p2.toPublicKey

        if (
          (pk1.bytes.head ^ pk2.bytes.head) == 0x01 && pk1.bytes.tail == pk2.bytes.tail
        ) {
          SecpPointInfinity
        } else {
          add(pk1, pk2).toPoint
        }
    }
  }

  /** Adds two public keys together, failing if the sum is 0x00 (the point at infinity). */
  def add(pk1: ECPublicKey, pk2: ECPublicKey): ECPublicKey

  /** Adds a Vector of public keys together, failing only if the total sum is 0x00
    * (the point at infinity), but still succeeding if sub-sums are 0x00.
    */
  def combinePubKeys(
      pubKeys: Vector[ECPublicKey],
      isCompressed: Boolean = true): ECPublicKey = {
    val summandPoints = pubKeys.map(_.toPoint)
    val sumPoint = summandPoints.reduce[SecpPoint](add(_, _))
    sumPoint match {
      case SecpPointInfinity =>
        throw new IllegalArgumentException(
          "Sum result was 0x00, an invalid public key.")
      case p: SecpPointFinite =>
        if (isCompressed) p.toPublicKey.compressed
        else p.toPublicKey.decompressed
    }
  }

  def pubKeyTweakAdd(pubkey: ECPublicKey, privkey: ECPrivateKey): ECPublicKey

  def isValidPubKey(pubKey: ECPublicKeyApi): Boolean = {
    pubKey.decompressedBytesT.isSuccess &&
    pubKey.decompressedBytesT.get != ByteVector(0x00)
  }

  def decodePoint(bytes: ByteVector): SecpPoint

  def decodePoint(pubKey: ECPublicKey): SecpPoint = {
    decodePoint(pubKey.decompressedBytes)
  }

  def schnorrSign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      auxRand: ByteVector): SchnorrDigitalSignature = {
    val nonceKey =
      SchnorrNonce.kFromBipSchnorr(privateKey, dataToSign, auxRand)

    schnorrSignWithNonce(dataToSign, privateKey, nonceKey)
  }

  def schnorrSignWithNonce(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey,
      nonceKey: ECPrivateKey): SchnorrDigitalSignature = {
    val rx = nonceKey.schnorrNonce
    val k = nonceKey.nonceKey.fieldElement
    val x = privateKey.schnorrKey.fieldElement
    val e = sha256SchnorrChallenge(
      rx.bytes ++ privateKey.schnorrPublicKey.bytes ++ dataToSign).bytes

    val challenge = x.multiply(FieldElement(e))
    val sig = k.add(challenge)

    SchnorrDigitalSignature(rx, sig)
  }

  def schnorrVerify(
      data: ByteVector,
      schnorrPubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature): Boolean = {
    val rx = signature.rx
    val sT = Try(signature.sig.toPrivateKey)

    sT match {
      case Success(s) =>
        val eBytes = sha256SchnorrChallenge(
          rx.bytes ++ schnorrPubKey.bytes ++ data).bytes

        val e = FieldElement(eBytes)
        val negE = e.negate

        val sigPoint = s.publicKey
        val challengePoint = schnorrPubKey.publicKey.multiply(negE)
        val computedR = challengePoint.toPoint.add(sigPoint.toPoint)
        computedR match {
          case SecpPointInfinity => false
          case point: SecpPointFinite =>
            !point.y.toBigInteger.testBit(0) && point.schnorrNonce == rx
        }
      case Failure(_) => false
    }
  }

  def schnorrComputeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      pubKey: SchnorrPublicKey,
      compressed: Boolean): ECPublicKey = {
    val eBytes = sha256SchnorrChallenge(
      nonce.bytes ++ pubKey.bytes ++ data).bytes

    val e = FieldElement(eBytes)

    val compressedSigPoint =
      nonce.publicKey.add(pubKey.publicKey.multiply(e))

    if (compressed) {
      compressedSigPoint
    } else {
      compressedSigPoint.decompressed
    }
  }

  def adaptorSign(
      key: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      msg: ByteVector,
      auxRand: ByteVector): ECAdaptorSignature = {
    AdaptorUtil.adaptorSign(key, adaptorPoint, msg, auxRand)
  }

  def adaptorComplete(
      key: ECPrivateKey,
      adaptorSignature: ECAdaptorSignature): ECDigitalSignature = {
    AdaptorUtil.adaptorComplete(key, adaptorSignature)
  }

  def extractAdaptorSecret(
      signature: ECDigitalSignature,
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey): ECPrivateKey = {
    AdaptorUtil.extractAdaptorSecret(signature, adaptorSignature, key)
  }

  def adaptorVerify(
      adaptorSignature: ECAdaptorSignature,
      key: ECPublicKey,
      msg: ByteVector,
      adaptorPoint: ECPublicKey): Boolean =
    AdaptorUtil.adaptorVerify(adaptorSignature, key, msg, adaptorPoint)

  def decodeSignature(signature: ECDigitalSignature): (BigInt, BigInt) =
    DERSignatureUtil.decodeSignature(signature)

  def isValidSignatureEncoding(signature: ECDigitalSignature): Boolean =
    DERSignatureUtil.isValidSignatureEncoding(signature)

  def isDEREncoded(signature: ECDigitalSignature): Boolean =
    DERSignatureUtil.isDEREncoded(signature)

  /** https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#hashing-data-objects */
  def sipHash(item: ByteVector, key: SipHashKey): Long

  def pbkdf2WithSha512(
      pass: String,
      salt: String,
      iterationCount: Int,
      derivedKeyLength: Int): ByteVector = {
    pbkdf2WithSha512(ByteVector(pass.getBytes),
                     ByteVector(salt.getBytes),
                     iterationCount,
                     derivedKeyLength)
  }

  def pbkdf2WithSha512(
      pass: ByteVector,
      salt: ByteVector,
      iterationCount: Int,
      derivedKeyLength: Int): ByteVector

  def randomBytes(n: Int): ByteVector

  /** Implements basic sanity tests for checking entropy like
    * making sure it isn't all the same bytes,
    * it isn't all 0x00...00
    * or it isn't all 0xffff...fff
    */
  def checkEntropy(bitVector: BitVector): Boolean = {
    val byteArr = bitVector.toByteArray
    if (bitVector.length < 128) {
      //not enough entropy
      false
    } else if (byteArr.toSet.size == 1) {
      //means all byte were the same
      //we need more diversity with entropy
      false
    } else {
      true
    }
  }

  def checkEntropy(bytes: ByteVector): Boolean = checkEntropy(bytes.toBitVector)
}
