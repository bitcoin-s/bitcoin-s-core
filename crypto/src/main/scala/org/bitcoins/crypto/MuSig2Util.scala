package org.bitcoins.crypto

import scodec.bits.ByteVector

// TODO static test vectors
// TODO implement tweaking
// TODO suppport js
// TODO test against secp256k1-zkp
// TODO refactor for niceness
object MuSig2Util {

  private val nonceNum: Int = 2

  case class MultiNoncePriv(privNonces: Vector[ECPrivateKey])
      extends NetworkElement {

    def toPublicNonces: MultiNoncePub = {
      MultiNoncePub(privNonces.map(_.publicKey))
    }

    def toFieldElements: Vector[FieldElement] = {
      privNonces.map(_.fieldElement)
    }

    def length: Int = privNonces.length

    override def bytes: ByteVector = {
      privNonces.map(_.bytes).reduce(_ ++ _)
    }

    def negate: MultiNoncePriv = {
      MultiNoncePriv(privNonces.map(_.negate))
    }
  }

  case class MultiNoncePub(pubNonces: Vector[ECPublicKey])
      extends NetworkElement {

    def apply(i: Int): ECPublicKey = {
      pubNonces(i)
    }

    def toPoints: Vector[SecpPoint] = {
      pubNonces.map(_.toPoint)
    }

    def length: Int = pubNonces.length

    override def bytes: ByteVector = {
      pubNonces.map(_.bytes).reduce(_ ++ _)
    }
  }

  case class KeyGenContext(
      aggPubKey: ECPublicKey,
      tacc: FieldElement,
      gacc: Boolean) {
    def getPubKey: ByteVector = aggPubKey.bytes.tail
    def getPubKey33: ByteVector = aggPubKey.bytes
  }

  case class KeySet(keys: Vector[SchnorrPublicKey]) {
    keys.init.zip(keys.tail).foreach { case (key1, key2) =>
      require(key1.hex.compareTo(key2.hex) <= 0,
              "Keys must be sorted lexicographically")
    }

    def apply(i: Int): SchnorrPublicKey = {
      keys(i)
    }

    lazy val serialize: ByteVector = {
      keys.map(_.bytes).reduce(_ ++ _)
    }

    def keyAggCoef(key: SchnorrPublicKey): FieldElement = {
      if (keys.length > 1 && key == secondKey) FieldElement.one
      else {
        val listHashBytes = aggListHash(serialize)
        val bytes = aggCoefHash(listHashBytes ++ key.bytes)

        FieldElement(new java.math.BigInteger(1, bytes.toArray))
      }
    }

    lazy val aggPubKey: ECPublicKey = {
      keys
        .map { key =>
          val coef = keyAggCoef(key)
          key.publicKey.multiply(coef)
        }
        .reduce(_.add(_))
    }

    lazy val secondKey: SchnorrPublicKey = keys.tail.head
  }

  object KeySet {

    def apply(keys: Vector[SchnorrPublicKey]): KeySet = {
      val sortedKeys = keys.sorted(NetworkElement.lexicographicalOrdering)
      new KeySet(sortedKeys)
    }

    def apply(keys: SchnorrPublicKey*): KeySet = {
      KeySet(keys.toVector)
    }
  }

  def aggListHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg list").bytes
  }

  def aggCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "KeyAgg coefficient").bytes
  }

  def nonHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/nonce").bytes
  }

  def nonCoefHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/noncecoef").bytes
  }

  def auxHash(bytes: ByteVector): ByteVector = {
    CryptoUtil.taggedSha256(bytes, "MuSig/aux").bytes
  }

  def genMultiNonce(
      privKeyOpt: Option[ECPrivateKey] = None,
      aggPubKeyOpt: Option[SchnorrPublicKey] = None,
      msgOpt: Option[ByteVector] = None,
      extraInOpt: Option[ByteVector] = None): (
      MultiNoncePub,
      MultiNoncePriv) = {
    msgOpt.foreach(msg => require(msg.length == 32))
    require(extraInOpt.forall(_.length <= 4294967295L))

    val preRand = CryptoUtil.randomBytes(32)
    val rand = privKeyOpt match {
      case Some(privKey) => auxHash(preRand).xor(privKey.bytes)
      case None          => preRand
    }

    val aggPubKeyBytes = aggPubKeyOpt match {
      case Some(aggPubKey) => aggPubKey.bytes.+:(aggPubKey.bytes.length.toByte)
      case None            => ByteVector.fromByte(0)
    }

    val msgBytes = msgOpt match {
      case Some(msg) => msg.+:(msg.length.toByte)
      case None      => ByteVector.fromByte(0)
    }

    val extraInBytes = extraInOpt match {
      case Some(extraIn) =>
        ByteVector.fromLong(extraIn.length).padLeft(4) ++ extraIn
      case None => ByteVector.fromByte(0)
    }

    val privNonceKeys = 0.until(nonceNum).toVector.map { index =>
      val indexByte = ByteVector.fromByte(index.toByte)

      val noncePreBytes =
        nonHash(rand ++ aggPubKeyBytes ++ msgBytes ++ extraInBytes ++ indexByte)

      val noncePreNum = new java.math.BigInteger(1, noncePreBytes.toArray)

      FieldElement(noncePreNum).toPrivateKey
    }
    val privNonces = MultiNoncePriv(privNonceKeys)
    val pubNonces = privNonces.toPublicNonces

    (pubNonces, privNonces)
  }

  def aggNonces(nonces: Vector[MultiNoncePub]): MultiNoncePub = {
    val aggNonceKeys = 0.until(nonceNum).toVector.map { i =>
      nonces.map(multiNonce => multiNonce(i)).reduce(_.add(_))
    }

    MultiNoncePub(aggNonceKeys)
  }

  private def nonceSum[T](
      nonces: Vector[T],
      b: FieldElement,
      add: (T, T) => T,
      multiply: (T, FieldElement) => T,
      identity: T): T = {
    nonces
      .foldLeft((FieldElement.one, identity)) { case ((pow, sumSoFar), nonce) =>
        val prod = multiply(nonce, pow)

        (pow.multiply(b), add(sumSoFar, prod))
      }
      ._2
  }

  def multiNoncePubSum(
      multiNoncePub: MultiNoncePub,
      b: FieldElement): ECPublicKey = {
    nonceSum[SecpPoint](multiNoncePub.toPoints,
                        b,
                        _.add(_),
                        _.multiply(_),
                        SecpPointInfinity) match {
      case SecpPointInfinity  => CryptoParams.getG
      case p: SecpPointFinite => p.toPublicKey
    }
  }

  def multiNoncePrivSum(
      multiNoncePriv: MultiNoncePriv,
      b: FieldElement): FieldElement = {
    nonceSum[FieldElement](multiNoncePriv.toFieldElements,
                           b,
                           _.add(_),
                           _.multiply(_),
                           FieldElement.zero)
  }

  def getSessionValues(
      aggMultiNoncePub: MultiNoncePub,
      keySet: KeySet,
      message: ByteVector): (FieldElement, ECPublicKey, FieldElement) = {
    require(aggMultiNoncePub.length == nonceNum)

    val aggPubKey = keySet.aggPubKey.schnorrPublicKey

    val bHash = nonCoefHash(
      aggMultiNoncePub.bytes ++ aggPubKey.bytes ++ message)
    val b = FieldElement(new java.math.BigInteger(1, bHash.toArray))

    val aggNonce = multiNoncePubSum(aggMultiNoncePub, b)
    val eBytes = CryptoUtil
      .sha256SchnorrChallenge(
        aggNonce.schnorrNonce.bytes ++ aggPubKey.bytes ++ message)
      .bytes
    val e = FieldElement(new java.math.BigInteger(1, eBytes.toArray))

    (b, aggNonce, e)
  }

  def sign(
      noncePriv: MultiNoncePriv,
      aggMultiNoncePub: MultiNoncePub,
      privKey: ECPrivateKey,
      message: ByteVector,
      keySet: KeySet): (ECPublicKey, FieldElement) = {
    val pubKey = privKey.publicKey
    val coef = keySet.keyAggCoef(pubKey.schnorrPublicKey)
    val (b, aggNonce, e) = getSessionValues(aggMultiNoncePub, keySet, message)

    val adjustedNoncePriv = aggNonce.parity match {
      case EvenParity => noncePriv
      case OddParity  => noncePriv.negate
    }

    val gp = pubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val g = keySet.aggPubKey.parity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    val adjustedPrivKey = privKey.fieldElement.multiply(gp).multiply(g)

    val privNonceSum = multiNoncePrivSum(adjustedNoncePriv, b)

    val s = adjustedPrivKey.multiply(e).multiply(coef).add(privNonceSum)

    require(
      partialSigVerify(s,
                       noncePriv.toPublicNonces,
                       pubKey.schnorrPublicKey,
                       keySet,
                       b,
                       aggNonce,
                       e))

    (aggNonce, s)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      multiNoncePub: MultiNoncePub,
      aggMultiNoncePub: MultiNoncePub,
      pubKey: SchnorrPublicKey,
      keySet: KeySet,
      message: ByteVector): Boolean = {
    val (b, aggNonce, e) = getSessionValues(aggMultiNoncePub, keySet, message)

    partialSigVerify(partialSig, multiNoncePub, pubKey, keySet, b, aggNonce, e)
  }

  def partialSigVerify(
      partialSig: FieldElement,
      multiNoncePub: MultiNoncePub,
      pubKey: SchnorrPublicKey,
      keySet: KeySet,
      b: FieldElement,
      aggNonce: ECPublicKey,
      e: FieldElement): Boolean = {
    val nonceSum = multiNoncePubSum(multiNoncePub, b)
    val nonceSumAdjusted = aggNonce.parity match {
      case EvenParity => nonceSum
      case OddParity  => nonceSum.multiply(FieldElement.orderMinusOne)
    }

    val aggKey = pubKey.toXOnly.publicKey(keySet.aggPubKey.parity)
    val a = keySet.keyAggCoef(pubKey)
    partialSig.getPublicKey == nonceSumAdjusted.add(
      aggKey.multiply(e.multiply(a)))
  }

  def signAgg(
      sVals: Vector[FieldElement],
      aggPubNonce: ECPublicKey): SchnorrDigitalSignature = {
    val s = sVals.reduce(_.add(_))

    SchnorrDigitalSignature(aggPubNonce.schnorrNonce, s)
  }
}
