package org.bitcoins.crypto

import scodec.bits.ByteVector

import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{BadPaddingException, Cipher, SecretKey}
import scala.util.{Failure, Success, Try}

/** Represents a encrypted cipher text with it's accompanying
  * initialization vector (IV). Both the cipher text and the IV
  * is needed to decrypt the cipher text.
  */
case class AesEncryptedData(cipherText: ByteVector, iv: AesIV)
    extends NetworkElement {

  /** We serialize IV and ciphertext by prepending the IV
    * to the ciphertext, and converting it to base64.
    * Since the IV is of static length, deserializing is a matter
    * of taking the first bytes as IV, and the rest as
    * ciphertext.
    */
  lazy val toBase64: String = {
    bytes.toBase64
  }

  /** The byte representation of the AesEncryptedData */
  override val bytes: ByteVector = iv.bytes ++ cipherText
}

object AesEncryptedData extends Factory[AesEncryptedData] {

  /** We serialize IV and ciphertext by prepending the IV
    * to the ciphertext, and converting it to base64.
    * Since the IV is of static length, deserializing is a matter
    * of taking the first bytes as IV, and the rest as
    * ciphertext.
    */
  def fromBase64(base64: String): Option[AesEncryptedData] = {
    ByteVector.fromBase64(base64) match {
      case None => None
      // we got passed valid base64, but it was too short
      // to be a proper encoding of AesEncryptedData
      case Some(bytes) if bytes.length <= AesIV.length => None
      case Some(bytes) =>
        Some(fromBytes(bytes))
    }
  }

  /** We serialize IV and ciphertext by prepending the IV
    * to the ciphertext, and converting it to base64.
    * Since the IV is of static length, deserializing is a matter
    * of taking the first bytes as IV, and the rest as
    * ciphertext.
    */
  def fromValidBase64(base64: String): AesEncryptedData =
    fromBase64(base64) match {
      case None =>
        throw new IllegalArgumentException(
          s"$base64 was not valid as AesEncryptedData!"
        )
      case Some(enc) => enc
    }

  /** Creates a AesEncryptedData out of a sequence of bytes. */
  override def fromBytes(bytes: ByteVector): AesEncryptedData = {
    require(
      bytes.length > AesIV.length,
      s"AesEncryptedData must be longer than ${AesIV.length} bytes, got $bytes")
    val (ivBytes, cipherText) = bytes.splitAt(AesIV.length)
    val iv = AesIV.fromValidBytes(ivBytes)
    AesEncryptedData(cipherText, iv)
  }
}

/** Represents a salt used to derive a AES key from
  * a human-readable passphrase.
  */
case class AesSalt(
    bytes: ByteVector
) extends AnyVal

object AesSalt extends Factory[AesSalt] {

  override def fromBytes(bytes: ByteVector): AesSalt = new AesSalt(bytes)

  /** Generates a random AES salt
    * of 32 bytes
    */
  def random: AesSalt = {
    val bytes = CryptoUtil.randomBytes(32)
    AesSalt(bytes)
  }
}

// we enforce the non-empty password length in the companion object
// to be able to make this extend AnyVal, and not be boxed at runtime
case class AesPassword private (private val value: String)
    extends MaskedToString {

  /** Converts this password into an AES key
    *
    * @return A tuple of the derived key and generated salt
    */
  def toKey: (AesKey, AesSalt) = {
    val salt = AesSalt.random
    val key = toKey(salt)

    (key, salt)
  }

  /** Given some salt, converts this password to an AES key
    * using PBKDF2 key stretching.
    */
  def toKey(salt: AesSalt): AesKey = {

    val passwordBytes = ByteVector.encodeUtf8(value) match {
      case Left(err)    => throw err
      case Right(bytes) => bytes
    }

    val secretKey = CryptoUtil.pbkdf2WithSha512(
      passwordBytes,
      salt.bytes,
      iterationCount = AesPassword.ITERATIONS,
      derivedKeyLength = AesPassword.KEY_SIZE)

    AesKey.fromValidBytes(secretKey)
  }

  override def toStringSensitive: String = value
}

object AesPassword extends StringFactory[AesPassword] {

  private val KEY_SIZE = 256
  private val ITERATIONS = 65536

  // to remove apply constructor from case class
  private[AesPassword] def apply(value: String) = new AesPassword(value)

  /** Tries to construct a password from the given string. Fails
    * if the string is empty.
    */
  override def fromStringOpt(string: String): Option[AesPassword] = {
    if (string.isEmpty) None else Some(AesPassword(string))
  }

  override def fromString(string: String): AesPassword = {
    fromStringOpt(string) match {
      case Some(password) => password
      case None =>
        sys.error(
          s"Could not construct AesPassword from given string, not logging in case it's sensitive")
    }
  }

  /** Constructs a password from the given string. Throws
    * if the string is empty.
    */
  def fromNonEmptyString(string: String): AesPassword =
    fromStringOpt(string).getOrElse(
      throw new IllegalArgumentException(
        "Cannot construct empty AES passwords!"))
}

/** Represents a encryption/decryption key.
  * AES keys can be converted to
  * [[javax.crypto.SecretKey SecretKey]]s,
  * and have certain length requirements.
  */
final case class AesKey private (bytes: ByteVector)
    extends MaskedToString
    with NetworkElement {

  /** The Java [[javax.crypto.SecretKey SecretKey]] representation
    * of this key.
    */
  def toSecretKey: SecretKey =
    new SecretKeySpec(bytes.toArray, "AES")

  override def toStringSensitive: String = {
    s"AesKey(${bytes.toHex})"
  }
}

object AesKey {

  // to remove apply constructor from case class
  private[AesKey] def apply(bytes: ByteVector): AesKey = new AesKey(bytes)

  /** Tries to construct an AES key from the
    * given bytes. AES keys have size constraints,
    * and must be 16, 24 or 32 bytes long.
    */
  def fromBytes(bytes: ByteVector): Option[AesKey] = {
    if (keylengths.exists(k => k == bytes.length)) {
      Some(AesKey(bytes))
    } else {
      None
    }
  }

  /** Constructs an AES key from a [[javax.crypto.SecretKey SecretKey]] */
  def fromSecretKey(key: SecretKey): AesKey = {
    val bytes = ByteVector(key.getEncoded)
    AesKey.fromValidBytes(bytes)
  }

  /** Construct a AES key from the given bytes,
    * throwing an exception if the provided bytes
    * are of invalid length.
    */
  def fromValidBytes(bytes: ByteVector): AesKey = {
    fromBytes(bytes).getOrElse(
      throw new IllegalArgumentException(
        s"Given bytes (${bytes.toHex}) had bad length"))
  }

  /** Allowed AES key lengths, bytes */
  private[crypto] val keylengths = List(16, 24, 32)

  /** Gets a AES key with the specified number of bytes */
  private def get(length: Int): AesKey = {
    AesKey(CryptoUtil.randomBytes(length))
  }

  /** Gets a random 128 bit AES key */
  def get128Bit(): AesKey = get(16)

  /** Gets a random 192 bit AES key */
  def get192Bit(): AesKey = get(24)

  /** Gets a random 256 bit AES key */
  def get256Bit(): AesKey = get(32)
}

/** Represents an initialization vector (IV) used
  * in AES encryption.
  */
final case class AesIV private (bytes: ByteVector)
    extends AnyVal
    with NetworkElement

object AesIV {

  /** Length of IV in bytes (for CFB mode, other modes have different lengths) */
  private[crypto] val length = 16

  // this is here to remove apply constructor
  private[AesIV] def apply(bytes: ByteVector): AesIV = new AesIV(bytes)

  /** Tries to construct an AES IV from the
    * given bytes. IVs must be 16 bytes long
    * (in CFB mode, which is what we use here).
    */
  def fromBytes(bytes: ByteVector): Option[AesIV] =
    if (bytes.length == AesIV.length) Some(new AesIV(bytes)) else None

  /** Constructs an AES IV from the given bytes. Throws if the given bytes are invalid */
  def fromValidBytes(bytes: ByteVector): AesIV =
    fromBytes(bytes).getOrElse(
      throw new IllegalArgumentException(
        s"Given bytes must be of length 16! Got: ${bytes.length}"))

  /** Generates a random IV */
  def random: AesIV = {
    AesIV(CryptoUtil.randomBytes(AesIV.length))
  }
}

/** Provides functionality for encrypting and decrypting with AES
  */
object AesCrypt {

  /** AES encryption with CFB block cipher mode
    * and no padding, such that arbitrary plaintexts
    * can be encrypted.
    */
  private val aesCipherType: String = "AES/CFB/NoPadding"

  private def getCipher: Cipher = {
    val cipher = Cipher.getInstance(aesCipherType)
    cipher
  }

  private def decryptionCipher(
      secret: AesKey,
      initializationVector: AesIV): Cipher = {
    val cipher = getCipher
    cipher.init(Cipher.DECRYPT_MODE,
                secret.toSecretKey,
                new IvParameterSpec(initializationVector.bytes.toArray))
    cipher
  }

  /** Decrypts the provided data
    */
  def decrypt(
      encrypted: AesEncryptedData,
      key: AesKey): Either[AesDecryptionException, ByteVector] = {
    val cipher = decryptionCipher(key, encrypted.iv)

    val decryptionAttempt = Try {
      val plainText = cipher.doFinal(encrypted.cipherText.toArray)
      ByteVector(plainText)
    }

    decryptionAttempt match {
      case Success(bytes) => Right(bytes)
      // here we assume that bad padding is because of a bad password
      // provided. assuming that our implementation is correct, correct
      // passwords should never to lead to bad padding
      case Failure(_: BadPaddingException) =>
        Left(AesException.BadPasswordException)
      case Failure(exception) => throw exception
    }
  }

  private def encryptionCipher(secret: AesKey, iv: AesIV): Cipher = {
    val cipher = getCipher
    cipher.init(Cipher.ENCRYPT_MODE,
                secret.toSecretKey,
                new IvParameterSpec(iv.bytes.toArray))
    cipher
  }

  /** Encrypts the given plaintext, by explicitly passing in a
    * intialization vector. This is unsafe if the user passes
    * in a bad IV, so this method is kept private within
    * Bitcoin-S. It is useful for testing purposes, so that's
    * why we expose it in the first place.
    */
  private[crypto] def encryptWithIV(
      plainText: ByteVector,
      iv: AesIV,
      key: AesKey): AesEncryptedData = {
    val cipher = encryptionCipher(key, iv)

    val cipherText = cipher.doFinal(plainText.toArray)

    val encrypted =
      AesEncryptedData(cipherText = ByteVector(cipherText), iv = iv)
    encrypted

  }

  /** Encrypts the given plaintext with the given key.
    */
  def encrypt(plainText: ByteVector, key: AesKey): AesEncryptedData = {
    val iv = AesIV.random
    encryptWithIV(plainText, iv, key)
  }
}

sealed trait AesDecryptionException extends Exception

object AesException {

  case object BadPasswordException
      extends BadPaddingException("Bad password provided for decryption")
      with AesDecryptionException

}
