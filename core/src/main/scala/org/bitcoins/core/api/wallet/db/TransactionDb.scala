package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

import org.bitcoins.core.currency.currencyUnitNumeric

trait TxDB {
  def txIdBE: DoubleSha256DigestBE
}

/** Represents a relevant transaction for the wallet that we should be keeping track of
  * @param txIdBE Transaction ID
  * @param transaction Serialized Transaction
  * @param unsignedTxIdBE Transaction ID of the unsigned transaction
  * @param unsignedTx Unsigned Transaction. This is useful so we can reconcile what our estimated
  *                   fees were against our actual fees in the case of ECDSA signature size variability
  * @param wTxIdBEOpt Witness Transaction ID
  * @param numInputs Number of inputs in the transaction
  * @param numOutputs Number of outputs in the transaction
  * @param lockTime locktime of the transaction
  */
case class TransactionDb(
    txIdBE: DoubleSha256DigestBE,
    transaction: Transaction,
    unsignedTxIdBE: DoubleSha256DigestBE,
    unsignedTx: Transaction,
    wTxIdBEOpt: Option[DoubleSha256DigestBE],
    totalOutput: CurrencyUnit,
    numInputs: Int,
    numOutputs: Int,
    lockTime: UInt32,
    blockHashOpt: Option[DoubleSha256DigestBE])
    extends TxDB {
  require(unsignedTx.inputs.forall(_.scriptSignature == EmptyScriptSignature),
          s"All ScriptSignatures must be empty, got $unsignedTx")

  lazy val txId: DoubleSha256Digest = txIdBE.flip
  lazy val unsignedTxId: DoubleSha256Digest = unsignedTxIdBE.flip
  lazy val wTxIdOpt: Option[DoubleSha256Digest] = wTxIdBEOpt.map(_.flip)
}

object TransactionDbHelper {

  def fromTransaction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]): TransactionDb = {
    val (unsignedTx, wTxIdBEOpt) = tx match {
      case btx: NonWitnessTransaction =>
        val unsignedInputs = btx.inputs.map(input =>
          TransactionInput(input.previousOutput,
                           EmptyScriptSignature,
                           input.sequence))
        (BaseTransaction(btx.version,
                         unsignedInputs,
                         btx.outputs,
                         btx.lockTime),
         None)
      case wtx: WitnessTransaction =>
        val unsignedInputs = wtx.inputs.map(input =>
          TransactionInput(input.previousOutput,
                           EmptyScriptSignature,
                           input.sequence))
        val uwtx = WitnessTransaction(wtx.version,
                                      unsignedInputs,
                                      wtx.outputs,
                                      wtx.lockTime,
                                      EmptyWitness.fromInputs(unsignedInputs))

        (uwtx, Some(uwtx.wTxIdBE))
    }
    val totalOutput = tx.outputs.map(_.value).sum
    TransactionDb(tx.txIdBE,
                  tx,
                  unsignedTx.txIdBE,
                  unsignedTx,
                  wTxIdBEOpt,
                  totalOutput,
                  tx.inputs.size,
                  tx.outputs.size,
                  tx.lockTime,
                  blockHashOpt)
  }
}
