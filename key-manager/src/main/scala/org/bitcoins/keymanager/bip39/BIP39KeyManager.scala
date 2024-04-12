package org.bitcoins.keymanager.bip39

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.keymanager.{
  BIP39KeyManagerApi,
  BIP39KeyManagerCreateApi,
  KeyManagerApi
}
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.{HDAccount, HDPath}
import org.bitcoins.core.util.{HDUtil, TimeUtil}
import org.bitcoins.core.wallet.keymanagement.KeyManagerUnlockError._
import org.bitcoins.core.wallet.keymanagement.{
  InitializeKeyManagerError,
  KeyManagerInitializeError,
  KeyManagerParams
}
import org.bitcoins.crypto.{AdaptorSign, AesPassword}
import org.bitcoins.keymanager._
import scodec.bits.BitVector

import java.nio.file.Files
import java.time.Instant
import scala.util.{Failure, Success, Try}

/** This is a key manager implementation meant to represent an in memory
  * BIP39 key manager
  *
  * @param rootExtPrivKey the root seed used for this wallet
  * @param kmParams the parameters used to generate the right keychain
  * @see https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
  */
class BIP39KeyManager(
    private val rootExtPrivKey: ExtPrivateKey,
    val kmParams: KeyManagerParams,
    val creationTime: Instant,
    val imported: Boolean)
    extends BIP39KeyManagerApi
    with KeyManagerLogger {

  override def equals(other: Any): Boolean =
    other match {
      case bip39Km: BIP39KeyManager =>
        getRootXPub == bip39Km.getRootXPub &&
          kmParams == bip39Km.kmParams &&
          creationTime.getEpochSecond == bip39Km.creationTime.getEpochSecond
      case _ =>
        other.equals(this)
    }

  /** Converts a non-sensitive DB representation of a UTXO into
    * a signable (and sensitive) real-world UTXO
    */
  def toSign(privKeyPath: HDPath): AdaptorSign = {
    val xpriv =
      rootExtPrivKey.deriveChildPrivKey(privKeyPath)

    xpriv
  }

  def deriveXPub(account: HDAccount): Try[ExtPublicKey] = {
    rootExtPrivKey.deriveChildPubKey(account)
  }

  /** Returns the root [[ExtPublicKey]] */
  val getRootXPub: ExtPublicKey = {
    rootExtPrivKey.extPublicKey
  }
}

object BIP39KeyManager
    extends BIP39KeyManagerCreateApi[BIP39KeyManager]
    with BitcoinSLogger {

  def fromMnemonic(
      mnemonic: MnemonicCode,
      kmParams: KeyManagerParams,
      bip39PasswordOpt: Option[String],
      creationTime: Instant,
      imported: Boolean
  ): BIP39KeyManager = {
    val seed = BIP39Seed.fromMnemonic(mnemonic, bip39PasswordOpt)
    val privVersion = HDUtil.getXprivVersion(kmParams.purpose, kmParams.network)
    val rootExtPrivKey = seed.toExtPrivateKey(privVersion)
    new BIP39KeyManager(rootExtPrivKey, kmParams, creationTime, imported)
  }

  val badPassphrase: AesPassword = AesPassword.fromString("changeMe")

  /** Initializes the mnemonic seed and saves it to file */
  override def initializeWithEntropy(
      aesPasswordOpt: Option[AesPassword],
      entropy: BitVector,
      bip39PasswordOpt: Option[String],
      kmParams: KeyManagerParams): Either[
    KeyManagerInitializeError,
    BIP39KeyManager] = {
    val seedPath = kmParams.seedPath
    logger.info(s"Initializing wallet with seedPath=$seedPath")

    val time = TimeUtil.now

    val writtenToDiskE: Either[KeyManagerInitializeError, KeyManagerApi] = {
      if (Files.notExists(seedPath)) {
        logger.info(
          s"Seed path parent directory does not exist, creating ${seedPath.getParent}")
        Files.createDirectories(seedPath.getParent)

        val mnemonicT = Try(MnemonicCode.fromEntropy(entropy))
        val mnemonicE: Either[KeyManagerInitializeError, MnemonicCode] =
          mnemonicT match {
            case Success(mnemonic) =>
              logger.info(s"Created mnemonic from entropy")
              Right(mnemonic)
            case Failure(err) =>
              logger.error(s"Could not create mnemonic from entropy! $err")
              Left(InitializeKeyManagerError.BadEntropy)
          }

        val writableMnemonicE: Either[KeyManagerInitializeError, SeedState] =
          mnemonicE.map { mnemonic =>
            val decryptedMnemonic =
              DecryptedMnemonic(mnemonic,
                                time,
                                backupTimeOpt = None,
                                imported = false)
            aesPasswordOpt match {
              case Some(aesPassword) => decryptedMnemonic.encrypt(aesPassword)
              case None =>
                decryptedMnemonic
            }
          }

        for {
          mnemonic <- mnemonicE
          writable <- writableMnemonicE
        } yield {
          val mnemonicPath =
            WalletStorage.writeSeedToDisk(seedPath, writable)
          logger.info(s"Saved wallet mnemonic to $mnemonicPath")

          fromMnemonic(mnemonic = mnemonic,
                       kmParams = kmParams,
                       bip39PasswordOpt = bip39PasswordOpt,
                       creationTime = time,
                       imported = writable.imported)
        }
      } else {
        logger.info(
          s"Seed file already exists, attempting to initialize form existing seed file=$seedPath.")

        WalletStorage.decryptSeedFromDisk(kmParams.seedPath,
                                          aesPasswordOpt) match {
          case Right(mnemonic: DecryptedMnemonic) =>
            Right(
              fromMnemonic(mnemonic = mnemonic.mnemonicCode,
                           kmParams = kmParams,
                           bip39PasswordOpt = bip39PasswordOpt,
                           creationTime = mnemonic.creationTime,
                           imported = mnemonic.imported))
          case Right(xprv: DecryptedExtPrivKey) =>
            val km = new BIP39KeyManager(xprv.xprv,
                                         kmParams,
                                         xprv.creationTime,
                                         xprv.imported)
            Right(km)
          case Left(err) =>
            Left(
              InitializeKeyManagerError.FailedToReadWrittenSeed(
                JsonParsingError(err.toString)))
        }
      }
    }

    //verify we can unlock it for a sanity check
    val unlocked = BIP39LockedKeyManager.unlock(passphraseOpt = aesPasswordOpt,
                                                bip39PasswordOpt =
                                                  bip39PasswordOpt,
                                                kmParams = kmParams)

    val biasedFinalE: Either[KeyManagerInitializeError, BIP39KeyManager] =
      for {
        kmBeforeWrite <- writtenToDiskE
        invariant <- unlocked match {
          case Right(unlockedKeyManager) =>
            require(
              unlockedKeyManager == kmBeforeWrite,
              s"We could not read the key manager we just wrote! $kmBeforeWrite != $unlockedKeyManager"
            )
            Right(unlockedKeyManager)

          case Left(err) =>
            Left(InitializeKeyManagerError.FailedToReadWrittenSeed(err))
        }
      } yield {
        invariant
      }

    biasedFinalE match {
      case Right(initSuccess) =>
        logger.info(s"Successfully initialized wallet")
        Right(initSuccess)
      case Left(err) =>
        logger.error(s"Failed to initialize key manager with err=$err")
        Left(err)
    }
  }

  /** Reads the key manager from disk and decrypts it with the given password */
  def fromParams(
      kmParams: KeyManagerParams,
      passwordOpt: Option[AesPassword],
      bip39PasswordOpt: Option[String]): Either[
    ReadMnemonicError,
    BIP39KeyManager] = {
    val mnemonicCodeE =
      WalletStorage.decryptSeedFromDisk(kmParams.seedPath, passwordOpt)

    mnemonicCodeE match {
      case Right(mnemonic: DecryptedMnemonic) =>
        Right(
          fromMnemonic(mnemonic.mnemonicCode,
                       kmParams,
                       bip39PasswordOpt,
                       mnemonic.creationTime,
                       mnemonic.imported))
      case Right(xprv: DecryptedExtPrivKey) =>
        val km = new BIP39KeyManager(xprv.xprv,
                                     kmParams,
                                     xprv.creationTime,
                                     xprv.imported)
        Right(km)
      case Left(v) => Left(v)
    }
  }
}
