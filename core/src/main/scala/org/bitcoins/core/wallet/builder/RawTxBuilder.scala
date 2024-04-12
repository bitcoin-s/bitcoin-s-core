package org.bitcoins.core.wallet.builder

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.transaction._

import scala.collection.mutable

/** The mutable transaction builder which collects:
  *   - Unsigned inputs (script signature will be ignored)
  *   - Destination outputs (change is dealt with in the finalizer)
  *   - A version number (default is [[TransactionConstants.validLockVersion]])
  *   - A lock time (default is [[TransactionConstants.lockTime]])
  *
  * At a high level, RawTxBuilder is responsible only for the funding inputs
  * and logical outputs (outputs that are intended by this transaction, and not
  * outputs computed from the logical outputs such as change outputs, which are
  * the responsibility of the RawTxFinalizer) of a transaction.
  *
  * RawTxBuilder supports inline calls to += and ++= for adding inputs
  * and outputs. Note that RawTxBuilder respects the order in which inputs
  * and outputs are added when generating a RawTxBuilderResult. If you wish
  * to have some other (computed) order, this is the responsibility of either
  * the calling code (to add in the correct order) or of the RawTxFinalizer which
  * may alter the order of inputs and outputs (and should only do so if it is
  * clearly documented that this will occur, otherwise it is safe to assume that
  * RawTxFinalizers will not alter the order of inputs and outputs).
  *
  * Once a transaction is done being built and is ready to be passed to a
  * RawTransactionFinalizer, call the result method to receive a
  * [[RawTxBuilderResult]] which can be passed into [[RawTxFinalizer.buildTx]].
  *
  * If you have access to a finalizer before you are ready to call result,
  * you may call the setFinalizer method to receive an instance of type
  * [[RawTxBuilderWithFinalizer]] which is described below, and where
  * you may continue to build and then call buildTx directly.
  *
  * Note: RawTxBuilder is not thread safe.
  */
case class RawTxBuilder() {
  private var version: Int32 = TransactionConstants.validLockVersion

  private val inputsBuilder: mutable.Builder[
    TransactionInput,
    Vector[TransactionInput]] = Vector.newBuilder

  private val outputsBuilder: mutable.Builder[
    TransactionOutput,
    Vector[TransactionOutput]] = Vector.newBuilder

  private var lockTime: UInt32 = TransactionConstants.lockTime

  /** Returns a RawTxBuilderResult ready for a RawTxFinalizer. */
  def result(): RawTxBuilderResult = {
    RawTxBuilderResult(version,
                       inputsBuilder.result(),
                       outputsBuilder.result(),
                       lockTime)
  }

  /** Returns a RawTxBuilderWithFinalizer where building can continue
    * and where buildTx can be called once building is completed.
    */
  def setFinalizer[F <: RawTxFinalizer](
      finalizer: F): RawTxBuilderWithFinalizer[F] = {
    RawTxBuilderWithFinalizer(this, finalizer)
  }

  /** Resets the RawTxBuilder as if it was just constructed. */
  def clear(): Unit = {
    version = TransactionConstants.validLockVersion
    inputsBuilder.clear()
    outputsBuilder.clear()
    lockTime = TransactionConstants.lockTime
  }

  /** Adds a TransactionInput to be the next input. No ScriptSignature is required
    * and any given ScriptSignature will be ignored (we recommend EmptyScriptSignature).
    */
  def addInput(input: TransactionInput): this.type = {
    inputsBuilder += input
    this
  }

  @inline final def +=(input: TransactionInput): this.type = addInput(input)

  def addInputs(inputs: Iterable[TransactionInput]): this.type = {
    inputsBuilder ++= inputs
    this
  }

  /** Adds a TransactionOuput to be the next output.
    *
    * Note that outputs like a change
    * output which are computed from other inputs and outputs should not be added here
    * and are instead the responsibility of the RawTxFinalizer.
    */
  def addOutput(output: TransactionOutput): this.type = {
    outputsBuilder += output
    this
  }

  @inline final def +=(output: TransactionOutput): this.type = addOutput(output)

  def addOutputs(outputs: Iterable[TransactionOutput]): this.type = {
    outputsBuilder ++= outputs
    this
  }

  /** Adds a collection of inputs and/or outputs to the
    * input and/or output lists
    */
  @inline final def ++=[T >: TransactionInput & TransactionOutput](
      inputsOrOutputs: Iterable[T]): this.type = {
    val vec = inputsOrOutputs.iterator.toVector
    val inputs = vec.collect { case input: TransactionInput =>
      input
    }
    val outputs = vec.collect { case output: TransactionOutput =>
      output
    }

    addInputs(inputs)
    addOutputs(outputs)
  }

  /** Sets the transaction version */
  def setVersion(version: Int32): this.type = {
    this.version = version
    this
  }

  /** Sets the transaction nLockTime */
  def setLockTime(lockTime: UInt32): this.type = {
    this.lockTime = lockTime
    this
  }
}

/** Wraps a RawTxBuilder and RawTxFinalizer pair.
  *
  * Provides access to builder methods for continuing
  * to collect inputs and outputs and also offers direct
  * access to the RawTxFinalizer's buildTx method which
  * completes the RawTxBuilder and then finalized the result.
  */
case class RawTxBuilderWithFinalizer[F <: RawTxFinalizer](
    builder: RawTxBuilder,
    finalizer: F) {

  /** Completes the builder and finalizes the result */
  def buildTx(): Transaction = {
    finalizer.buildTx(builder.result())
  }

  def clearBuilder(): Unit = {
    builder.clear()
  }

  @inline final def +=(input: TransactionInput): this.type = {
    builder += input
    this
  }

  @inline final def +=(output: TransactionOutput): this.type = {
    builder += output
    this
  }

  @inline final def ++=[T >: TransactionInput & TransactionOutput](
      inputsOrOutputs: Iterable[T]): this.type = {
    builder ++= inputsOrOutputs
    this
  }

  def setVersion(version: Int32): this.type = {
    builder.setVersion(version)
    this
  }

  def setLockTime(lockTime: UInt32): this.type = {
    builder.setLockTime(lockTime)
    this
  }
}

object RawTxBuilderWithFinalizer {

  def apply[F <: RawTxFinalizer](finalizer: F): RawTxBuilderWithFinalizer[F] = {
    RawTxBuilderWithFinalizer(RawTxBuilder(), finalizer)
  }
}
