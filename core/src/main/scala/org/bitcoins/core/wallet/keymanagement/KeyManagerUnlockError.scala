package org.bitcoins.core.wallet.keymanagement

sealed trait KeyManagerUnlockError extends Error

object KeyManagerUnlockError {

  case object MnemonicNotFound
      extends Error("Mnemonic not found")
      with KeyManagerUnlockError

  case object BadPassword
      extends Error("Bad password for unlocking wallet!")
      with KeyManagerUnlockError

  case class JsonParsingError(message: String)
      extends Error(message)
      with KeyManagerUnlockError
}
