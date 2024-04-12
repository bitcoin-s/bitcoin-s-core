package org.bitcoins.commons.jsonmodels.bitcoind

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.duration.FiniteDuration

sealed abstract class RawTransactionResult

sealed abstract class RpcTransaction extends RawTransactionResult {
  def txid: DoubleSha256DigestBE
  def hash: DoubleSha256DigestBE
  def version: Int
  def size: Int
  def vsize: Int
  def locktime: UInt32
  def vin: Vector[TransactionInput]
  def vout: Vector[RpcTransactionOutput]
  def hex: Option[Transaction]
}

case class RpcTransactionPreV22(
    txid: DoubleSha256DigestBE,
    hash: DoubleSha256DigestBE,
    version: Int,
    size: Int,
    vsize: Int,
    locktime: UInt32,
    vin: Vector[TransactionInput],
    vout: Vector[RpcTransactionOutputPreV22],
    hex: Option[Transaction])
    extends RpcTransaction

case class RpcTransactionV22(
    txid: DoubleSha256DigestBE,
    hash: DoubleSha256DigestBE,
    version: Int,
    size: Int,
    vsize: Int,
    locktime: UInt32,
    vin: Vector[TransactionInput],
    vout: Vector[RpcTransactionOutputV22],
    hex: Option[Transaction])
    extends RpcTransaction

sealed trait RpcTransactionOutput extends RawTransactionResult {
  def value: Bitcoins
  def n: Int
  def scriptPubKey: RpcScriptPubKey
}

case class RpcTransactionOutputPreV22(
    value: Bitcoins,
    n: Int,
    scriptPubKey: RpcScriptPubKeyPreV22)
    extends RpcTransactionOutput

case class RpcTransactionOutputV22(
    value: Bitcoins,
    n: Int,
    scriptPubKey: RpcScriptPubKeyPostV22)
    extends RpcTransactionOutput

sealed trait RpcScriptPubKey extends RawTransactionResult {
  def asm: String
  def hex: String
  def scriptType: ScriptType
  def addresses: Option[Vector[BitcoinAddress]]
}

case class RpcScriptPubKeyPreV22(
    asm: String,
    hex: String,
    reqSigs: Option[Int],
    scriptType: ScriptType,
    addresses: Option[Vector[BitcoinAddress]])
    extends RpcScriptPubKey

case class RpcScriptPubKeyPostV22(
    asm: String,
    hex: String,
    scriptType: ScriptType,
    addresses: Option[Vector[BitcoinAddress]],
    address: Option[BitcoinAddress])
    extends RpcScriptPubKey

sealed trait DecodeScriptResult extends RawTransactionResult {
  def asm: String
  def typeOfScript: Option[ScriptType]
  def p2sh: P2SHAddress
}

case class DecodeScriptResultPreV22(
    asm: String,
    typeOfScript: Option[ScriptType],
    reqSigs: Option[Int],
    addresses: Option[Vector[P2PKHAddress]],
    p2sh: P2SHAddress)
    extends DecodeScriptResult

case class DecodeScriptResultV22(
    asm: String,
    typeOfScript: Option[ScriptType],
    p2sh: P2SHAddress)
    extends DecodeScriptResult

case class FundRawTransactionResult(
    hex: Transaction,
    fee: Bitcoins,
    changepos: Int)
    extends RawTransactionResult

case class SignRawTransactionWithWalletResult(
    hex: Transaction,
    complete: Boolean
)

sealed trait GetRawTransactionResult extends RawTransactionResult {
  def in_active_blockchain: Option[Boolean]
  def hex: Transaction
  def txid: DoubleSha256DigestBE
  def hash: DoubleSha256DigestBE
  def size: Int
  def vsize: Int
  def version: Int
  def locktime: UInt32
  def vin: Vector[GetRawTransactionVin]
  def vout: Vector[RpcTransactionOutput]
  def blockhash: Option[DoubleSha256DigestBE]
  def confirmations: Option[Int]
  def time: Option[UInt32]
  def blocktime: Option[UInt32]
}

case class GetRawTransactionResultPreV22(
    in_active_blockchain: Option[Boolean],
    hex: Transaction,
    txid: DoubleSha256DigestBE,
    hash: DoubleSha256DigestBE,
    size: Int,
    vsize: Int,
    version: Int,
    locktime: UInt32,
    vin: Vector[GetRawTransactionVin],
    vout: Vector[RpcTransactionOutputPreV22],
    blockhash: Option[DoubleSha256DigestBE],
    confirmations: Option[Int],
    time: Option[UInt32],
    blocktime: Option[UInt32])
    extends GetRawTransactionResult

case class GetRawTransactionResultV22(
    in_active_blockchain: Option[Boolean],
    hex: Transaction,
    txid: DoubleSha256DigestBE,
    hash: DoubleSha256DigestBE,
    size: Int,
    vsize: Int,
    version: Int,
    locktime: UInt32,
    vin: Vector[GetRawTransactionVin],
    vout: Vector[RpcTransactionOutputV22],
    blockhash: Option[DoubleSha256DigestBE],
    confirmations: Option[Int],
    time: Option[UInt32],
    blocktime: Option[UInt32])
    extends GetRawTransactionResult

case class GetRawTransactionVin(
    txid: Option[DoubleSha256DigestBE],
    vout: Option[Int],
    scriptSig: Option[GetRawTransactionScriptSig],
    sequence: Option[BigDecimal],
    txinwitness: Option[Vector[String]] // Should be TransactionWitness?
) extends RawTransactionResult

case class GetRawTransactionScriptSig(asm: String, hex: ScriptSignature)
    extends RawTransactionResult

case class SignRawTransactionResult(
    hex: Transaction,
    complete: Boolean,
    errors: Option[Vector[SignRawTransactionError]])
    extends RawTransactionResult

case class SignRawTransactionError(
    txid: DoubleSha256DigestBE,
    vout: Int,
    scriptSig: ScriptPubKey,
    sequence: UInt32,
    error: String)
    extends RawTransactionResult

case class GetRpcInfoResult(
    active_commands: Vector[RpcCommands]
) extends RawTransactionResult

case class RpcCommands(
    method: String,
    duration: FiniteDuration //this time is in microseconds
) extends RawTransactionResult
