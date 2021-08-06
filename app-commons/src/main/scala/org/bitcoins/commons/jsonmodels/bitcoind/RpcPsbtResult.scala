package org.bitcoins.commons.jsonmodels.bitcoind

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.{
  ECDigitalSignature,
  ECPublicKey,
  ECPublicKeyBytes,
  HashType
}

sealed abstract class RpcPsbtResult

case class WalletProcessPsbtResult(psbt: PSBT, complete: Boolean)
    extends RpcPsbtResult

sealed abstract class FinalizePsbtResult extends RpcPsbtResult
case class FinalizedPsbt(hex: Transaction) extends FinalizePsbtResult
case class NonFinalizedPsbt(psbt: PSBT) extends FinalizePsbtResult

sealed abstract class DecodePsbtResult extends RpcPsbtResult {
  def tx: RpcTransaction
  def unknown: Map[String, String]
  def inputs: Vector[RpcPsbtInput]
  def outputs: Vector[RpcPsbtOutput]
  def fee: Option[Bitcoins]
}

case class DecodePsbtResultPreV22(
    tx: RpcTransactionPreV22,
    unknown: Map[String, String],
    inputs: Vector[RpcPsbtInputPreV22],
    outputs: Vector[RpcPsbtOutput],
    fee: Option[Bitcoins])
    extends DecodePsbtResult

final case class DecodePsbtResultV22(
    tx: RpcTransactionV22,
    unknown: Map[String, String],
    inputs: Vector[RpcPsbtInputV22],
    outputs: Vector[RpcPsbtOutput],
    fee: Option[Bitcoins])
    extends DecodePsbtResult

sealed abstract class RpcPsbtInput extends RpcPsbtResult {
  def nonWitnessUtxo: Option[RpcTransaction]
  def witnessUtxo: Option[PsbtWitnessUtxoInput]
  def partialSignatures: Option[Map[ECPublicKey, ECDigitalSignature]]
  def sighash: Option[HashType]
  def redeemScript: Option[RpcPsbtScript]
  def witnessScript: Option[RpcPsbtScript]
  def bip32Derivs: Option[Vector[PsbtBIP32Deriv]]
  def finalScriptSig: Option[RpcPsbtScript]

  def finalScriptwitness: Option[
    Vector[String]
  ] // todo(torkelrogstad) needs example of what this looks like
  def unknown: Option[Map[String, String]] // The unknown global fields
}

case class RpcPsbtInputPreV22(
    nonWitnessUtxo: Option[RpcTransactionPreV22],
    witnessUtxo: Option[PsbtWitnessUtxoInput],
    partialSignatures: Option[Map[ECPublicKey, ECDigitalSignature]],
    sighash: Option[HashType],
    redeemScript: Option[RpcPsbtScript],
    witnessScript: Option[RpcPsbtScript],
    bip32Derivs: Option[Vector[PsbtBIP32Deriv]],
    finalScriptSig: Option[RpcPsbtScript],
    finalScriptwitness: Option[
      Vector[String]
    ], // todo(torkelrogstad) needs example of what this looks like
    unknown: Option[Map[String, String]] // The unknown global fields
) extends RpcPsbtInput

final case class RpcPsbtInputV22(
    nonWitnessUtxo: Option[RpcTransactionV22],
    witnessUtxo: Option[PsbtWitnessUtxoInput],
    partialSignatures: Option[Map[ECPublicKey, ECDigitalSignature]],
    sighash: Option[HashType],
    redeemScript: Option[RpcPsbtScript],
    witnessScript: Option[RpcPsbtScript],
    bip32Derivs: Option[Vector[PsbtBIP32Deriv]],
    finalScriptSig: Option[RpcPsbtScript],
    finalScriptwitness: Option[
      Vector[String]
    ], // todo(torkelrogstad) needs example of what this looks like
    unknown: Option[Map[String, String]] // The unknown global fields
) extends RpcPsbtInput

case class RpcPsbtScript(
    asm: String, // todo(torkelrogstad) split into Vector[ScriptToken]?
    hex: ScriptPubKey,
    scriptType: Option[ScriptType],
    address: Option[BitcoinAddress]
) extends RpcPsbtResult

case class PsbtBIP32Deriv(
    pubkey: ECPublicKey,
    masterFingerprint: String, // todo(torkelrogstad)
    path: String
    // todo(torkelrogstad) there's more fields here
) extends RpcPsbtResult

case class PsbtWitnessUtxoInput(
    amount: Bitcoins,
    scriptPubKey: RpcPsbtScript
) extends RpcPsbtResult

case class RpcPsbtOutput(
    redeemScript: Option[RpcPsbtScript],
    witnessScript: Option[RpcPsbtScript],
    bip32Derivs: Option[Vector[PsbtBIP32Deriv]],
    unknown: Option[Map[String, String]]
) extends RpcPsbtResult

case class WalletCreateFundedPsbtResult(
    psbt: PSBT,
    fee: Bitcoins,
    changepos: Int
) extends RpcPsbtResult

case class AnalyzePsbtResult(
    inputs: Vector[AnalyzePsbtInput],
    estimated_vsize: Option[Double],
    estimated_feerate: Option[Double],
    fee: Option[Bitcoins],
    next: String
) extends RpcPsbtResult

case class AnalyzePsbtInput(
    has_utxo: Boolean,
    is_final: Boolean,
    missing: Option[PsbtMissingData],
    next: Option[String]
) extends RpcPsbtResult

case class PsbtMissingData(
    pubkeys: Option[Vector[ECPublicKeyBytes]],
    signatures: Option[Vector[ECDigitalSignature]],
    redeemscript: Option[RpcPsbtScript],
    witnessscript: Option[RpcPsbtScript]
) extends RpcPsbtResult
