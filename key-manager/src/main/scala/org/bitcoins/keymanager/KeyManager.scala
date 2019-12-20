package org.bitcoins.keymanager

import java.nio.file.Files

import org.bitcoins.core.compat.{CompatEither, CompatLeft, CompatRight}
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.{HDAccount, HDPath}
import org.bitcoins.core.util.BitcoinSLogger
import scodec.bits.BitVector

import scala.util.{Failure, Success, Try}

case class KeyManager(
    private val mnemonic: MnemonicCode,
    kmParams: KeyManagerParams) {

  private[keymanager] val seed = BIP39Seed.fromMnemonic(
    mnemonic,
    BIP39Seed.EMPTY_PASSWORD) // todo think more about this

  private val privVersion: ExtKeyPrivVersion =
    HDUtil.getXprivVersion(kmParams.purpose, kmParams.network)

  private[keymanager] val rootExtPrivKey = seed.toExtPrivateKey(privVersion)

  /** Converts a non-sensitive DB representation of a UTXO into
    * a signable (and sensitive) real-world UTXO
    */
  def toSign(privKeyPath: HDPath): Sign = {
    val xpriv =
      rootExtPrivKey.deriveChildPrivKey(privKeyPath)
    val privKey = xpriv.key
    val pubAtPath = privKey.publicKey

    val sign: Sign = Sign(privKey.signFunction, pubAtPath)

    sign
  }

  def deriveXPub(account: HDAccount): Try[ExtPublicKey] = {
    rootExtPrivKey.deriveChildPubKey(account)
  }

  def getXPub: ExtPublicKey = { rootExtPrivKey.extPublicKey }
}

object KeyManager extends CreateKeyManagerApi with BitcoinSLogger {
  val badPassphrase = AesPassword.fromString("bad-password").get

  /** Initializes the mnemonic seed and saves it to file */
  override def initializeWithEntropy(
      entropy: BitVector,
      kmParams: KeyManagerParams): InitializeKeyManagerResult = {
    val seedPath = kmParams.seedPath
    logger.info(s"Initializing wallet with seedPath=${seedPath}")

    if (Files.notExists(seedPath)) {
      logger.info(
        s"Seed path parent directory does not exist, creating ${seedPath.getParent}")
      Files.createDirectories(seedPath.getParent)
    }

    val mnemonicT = Try(MnemonicCode.fromEntropy(entropy))
    val mnemonicE: CompatEither[InitializeKeyManagerError, MnemonicCode] =
      mnemonicT match {
        case Success(mnemonic) =>
          logger.info(s"Created mnemonic from entropy")
          CompatEither(Right(mnemonic))
        case Failure(err) =>
          logger.error(s"Could not create mnemonic from entropy! $err")
          CompatEither(Left(InitializeKeyManagerError.BadEntropy))
      }

    val encryptedMnemonicE: CompatEither[
      InitializeKeyManagerError,
      EncryptedMnemonic] =
      mnemonicE.map { EncryptedMnemonicHelper.encrypt(_, badPassphrase) }

    val writeToDiskE: CompatEither[InitializeKeyManagerError, KeyManager] =
      for {
        mnemonic <- mnemonicE
        encrypted <- encryptedMnemonicE
        _ = {
          val mnemonicPath =
            WalletStorage.writeMnemonicToDisk(seedPath, encrypted)
          logger.info(s"Saved encrypted wallet mnemonic to $mnemonicPath")
        }

      } yield KeyManager(mnemonic = mnemonic, kmParams = kmParams)

    //verify we can unlock it for a sanity check
    val unlocked = LockedKeyManager.unlock(badPassphrase, kmParams)

    val biasedFinalE: CompatEither[
      InitializeKeyManagerError,
      InitializeKeyManagerSuccess] = for {
      kmBeforeWrite <- writeToDiskE
      invariant <- unlocked match {
        case UnlockKeyManagerSuccess(unlockedKeyManager) =>
          require(kmBeforeWrite == unlockedKeyManager,
                  s"We could not read the key manager we just wrote!")
          CompatRight(InitializeKeyManagerSuccess(unlockedKeyManager))

        case err: UnlockKeyManagerError =>
          CompatLeft(InitializeKeyManagerError.FailedToReadWrittenSeed(err))
      }
    } yield {
      invariant
    }

    biasedFinalE match {
      case CompatRight(initSuccess) =>
        logger.info(s"Successfully initialized wallet")
        initSuccess
      case CompatLeft(err) =>
        logger.error(s"Failed to initialize key manager with err=${err}")
        err
    }
  }

  /** Reads the key manager from disk and decrypts it with the given password */
  def fromParams(
      kmParams: KeyManagerParams,
      password: AesPassword): Either[ReadMnemonicError, KeyManager] = {
    val mnemonicCodeE =
      WalletStorage.decryptMnemonicFromDisk(kmParams.seedPath, password)

    mnemonicCodeE match {
      case r: ReadMnemonicSuccess =>
        Right(new KeyManager(r.mnemonic, kmParams))
      case e: ReadMnemonicError =>
        Left(e)
    }
  }
}
