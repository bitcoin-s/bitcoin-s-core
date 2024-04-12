package org.bitcoins.core.wallet.keymanagement

sealed trait KeyManagerInitializeError extends Error

object InitializeKeyManagerError {

  // todo add explanation of what good/bad entropy is
  case object BadEntropy
      extends Error("Bad Entropy")
      with KeyManagerInitializeError

  case class EncryptionError(underlying: Throwable)
      extends Error(underlying)
      with KeyManagerInitializeError

  /** Wallet data was found in the
    * specified data directory.
    *
    * @note `bitcoin-s` only supports one wallet
    *   per network at the moment.
    */
  case object WalletAlreadyExists
      extends Error("Wallet already exists")
      with KeyManagerInitializeError

  case class FailedToReadWrittenSeed(unlockErr: KeyManagerUnlockError)
      extends Error(s"We failed to read the mnemonic seed we just wrote")
      with KeyManagerInitializeError
}
