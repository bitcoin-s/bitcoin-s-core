package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{EmptyScriptWitness, ScriptWitness}
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.core.wallet.builder.RawTxBuilder
import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Created by chris on 7/14/15.
  */
sealed abstract class Transaction extends NetworkElement {

  /** The `sha256(sha256(tx))` of this transaction,
    * Note that this is the little endian encoding of the hash, NOT the big endian encoding shown in block
    * explorers. See
    * [[https://bitcoin.stackexchange.com/questions/2063/why-does-the-bitcoin-protocol-use-the-little-endian-notation this link]]
    * for more info
    */
  def txId: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  /** This is the BIG ENDIAN encoding for the txid. This is commonly used for
    * RPC interfaces and block explorers, this encoding is NOT used at the protocol level
    * For more info see:
    * [[https://bitcoin.stackexchange.com/questions/2063/why-does-the-bitcoin-protocol-use-the-little-endian-notation]]
    */
  def txIdBE: DoubleSha256DigestBE = txId.flip

  /** The version number for this transaction */
  def version: Int32

  /** The inputs for this transaction */
  def inputs: Seq[TransactionInput]

  /** The outputs for this transaction */
  def outputs: Seq[TransactionOutput]

  /** The locktime for this transaction */
  def lockTime: UInt32

  /** This is used to indicate how 'expensive' the transction is on the blockchain.
    * This use to be a simple calculation before segwit (BIP141). Each byte in the transaction
    * counted as 4 'weight' units. Now with segwit, the
    * [[org.bitcoins.core.protocol.transaction.TransactionWitness TransactionWitness]]
    * is counted as 1 weight unit per byte,
    * while other parts of the transaction (outputs, inputs, locktime etc) count as 4 weight units.
    * As we add more witness versions, this may be subject to change.
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations BIP 141]]
    * [[https://github.com/bitcoin/bitcoin/blob/5961b23898ee7c0af2626c46d5d70e80136578d3/src/consensus/validation.h#L96]]
    */
  def weight: Long

  /** The transaction's virtual size
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations]]
    */
  def vsize: Long = Math.ceil(weight / 4.0).toLong

  /** Base transaction size is the size of the transaction serialised with the witness data stripped
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations]]
    */
  def baseSize: Long =
    this match {
      case btx: NonWitnessTransaction => btx.byteSize
      case wtx: WitnessTransaction =>
        BaseTransaction(wtx.version,
                        wtx.inputs,
                        wtx.outputs,
                        wtx.lockTime).baseSize
    }

  def totalSize: Long = bytes.size

  /** Determines if this transaction is a coinbase transaction. */
  def isCoinbase: Boolean =
    inputs.size match {
      case 1 =>
        inputs.head match {
          case _: CoinbaseInput    => true
          case _: TransactionInput => false
        }
      case _: Int => false
    }

  /** Updates the input at the given index and returns the new transaction with that input updated */
  def updateInput(idx: Int, i: TransactionInput): Transaction = {
    val updatedInputs = inputs.updated(idx, i)
    this match {
      case _: NonWitnessTransaction =>
        BaseTransaction(version, updatedInputs, outputs, lockTime)
      case wtx: WitnessTransaction =>
        WitnessTransaction(version,
                           updatedInputs,
                           outputs,
                           lockTime,
                           wtx.witness)
    }
  }

  lazy val toBaseTx: BaseTransaction = {
    BaseTransaction(version, inputs, outputs, lockTime)
  }

  lazy val totalOutput: CurrencyUnit =
    outputs.map(_.value).sum(org.bitcoins.core.currency.currencyUnitNumeric)
}

object Transaction extends Factory[Transaction] {
  def newBuilder: RawTxBuilder = RawTxBuilder()

  override def fromBytes(bytes: ByteVector): Transaction = {
    //see BIP141 for marker/flag bytes
    //https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#transaction-id
    val tx = {
      if (
        bytes(4) == WitnessTransaction.marker && bytes(
          5) == WitnessTransaction.flag
      ) {
        //this throw/catch is _still_ necessary for the case where we have unsigned base transactions
        //with zero inputs and 1 output which is serialized as "0001" at bytes 4 and 5.
        //these transactions will not have a script witness associated with them making them invalid
        //witness transactions (you need to have a witness to be considered a witness tx)
        //see: https://github.com/bitcoin-s/bitcoin-s/blob/01d89df1b7c6bc4b1594406d54d5e6019705c654/core-test/src/test/scala/org/bitcoins/core/protocol/transaction/TransactionTest.scala#L88
        try {
          WitnessTransaction.fromBytes(bytes)
        } catch {
          case scala.util.control.NonFatal(_) =>
            BaseTransaction.fromBytes(bytes)
        }
      } else {
        BaseTransaction.fromBytes(bytes)
      }
    }
    tx
  }
}

sealed abstract class NonWitnessTransaction extends Transaction {
  override def weight: Long = byteSize * 4

  override val bytes: ByteVector = {
    val versionBytes = version.bytes.reverse
    val inputBytes = BytesUtil.writeCmpctSizeUInt(inputs)
    val outputBytes = BytesUtil.writeCmpctSizeUInt(outputs)
    val lockTimeBytes = lockTime.bytes.reverse

    versionBytes ++ inputBytes ++ outputBytes ++ lockTimeBytes
  }

  override val txId: DoubleSha256Digest = super.txId
}

case class BaseTransaction(
    version: Int32,
    inputs: Seq[TransactionInput],
    outputs: Seq[TransactionOutput],
    lockTime: UInt32)
    extends NonWitnessTransaction

object BaseTransaction extends Factory[BaseTransaction] {

  override def fromBytes(bytes: ByteVector): BaseTransaction = {
    val versionBytes = bytes.take(4)
    val version = Int32(versionBytes.reverse)
    val txInputBytes = bytes.slice(4, bytes.size)
    val (inputs, outputBytes) =
      BytesUtil.parseCmpctSizeUIntSeq(txInputBytes, TransactionInput)
    val (outputs, lockTimeBytes) =
      BytesUtil.parseCmpctSizeUIntSeq(outputBytes, TransactionOutput)
    val lockTime = UInt32(lockTimeBytes.take(4).reverse)

    BaseTransaction(version, inputs, outputs, lockTime)
  }

  def unapply(tx: NonWitnessTransaction): Option[
    (Int32, Seq[TransactionInput], Seq[TransactionOutput], UInt32)] = {
    Some((tx.version, tx.inputs, tx.outputs, tx.lockTime))
  }
}

case object EmptyTransaction extends NonWitnessTransaction {
  override val txId: DoubleSha256Digest = DoubleSha256Digest.empty
  override def version: Int32 = TransactionConstants.version
  override def inputs: Vector[TransactionInput] = Vector.empty
  override def outputs: Vector[TransactionOutput] = Vector.empty
  override def lockTime: UInt32 = TransactionConstants.lockTime
}

case class WitnessTransaction(
    version: Int32,
    inputs: Seq[TransactionInput],
    outputs: Seq[TransactionOutput],
    lockTime: UInt32,
    witness: TransactionWitness)
    extends Transaction {
  require(
    inputs.length == witness.length,
    s"Must have same amount of inputs and witnesses in witness tx, inputs=${inputs.length} witnesses=${witness.length}"
  )

  /** The txId for the witness transaction from satoshi's original serialization */
  override def txId: DoubleSha256Digest = {
    toBaseTx.txId
  }

  /** The witness transaction id as defined by
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#transaction-id BIP141]]
    */
  def wTxId: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  /** Returns the big endian encoding of the wtxid */
  def wTxIdBE: DoubleSha256DigestBE = wTxId.flip

  /** Weight calculation in bitcoin for witness txs
    * [[https://github.com/bitcoin/bitcoin/blob/5961b23898ee7c0af2626c46d5d70e80136578d3/src/consensus/validation.h#L96]]
    */
  override def weight: Long = {
    toBaseTx.byteSize * 3 + byteSize
  }

  /** Writes a [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] to a hex string
    * This is unique from BaseTransaction.bytes in the fact
    * that it adds a 'marker' and 'flag' to indicate that this tx is a
    * [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] and has extra
    * witness data attached to it.
    * See [[https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki BIP144]] for more info.
    * Functionality inside of Bitcoin Core:
    * [[https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L282-L287s]]
    */
  override val bytes: ByteVector = {
    val versionBytes = version.bytes.reverse
    val inputBytes = BytesUtil.writeCmpctSizeUInt(inputs)
    val outputBytes = BytesUtil.writeCmpctSizeUInt(outputs)
    val witnessBytes = witness.bytes
    val lockTimeBytes = lockTime.bytes.reverse
    // notice we use the old serialization format if all witnesses are empty
    // https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L276-L281
    if (witness.exists(_ != EmptyScriptWitness)) {
      val witConstant = ByteVector(0.toByte, 1.toByte)
      versionBytes ++ witConstant ++ inputBytes ++ outputBytes ++ witnessBytes ++ lockTimeBytes
    } else toBaseTx.bytes
  }

  /** Updates the [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]] at the given index and
    * returns a new [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]
    * with it's witness vector updated
    */
  def updateWitness(idx: Int, scriptWit: ScriptWitness): WitnessTransaction = {
    val txWit = witness.updated(idx, scriptWit)
    WitnessTransaction(version, inputs, outputs, lockTime, txWit)
  }
}

object WitnessTransaction extends Factory[WitnessTransaction] {

  /** This read function is unique to BaseTransaction.fromBytes
    * in the fact that it reads a 'marker' and 'flag' byte to indicate that this tx is a
    * [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]].
    * See [[https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki BIP144 ]] for more details.
    * Functionality inside of Bitcoin Core:
    * [[https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L244-L251]]
    */
  override def fromBytes(bytes: ByteVector): WitnessTransaction = {
    val versionBytes = bytes.take(4)
    val version = Int32(versionBytes.reverse)
    val marker = bytes(4)
    require(
      marker.toInt == 0,
      "Incorrect marker for witness transaction, the marker MUST be 0 for the marker according to BIP141, got: " + marker)
    val flag = bytes(5)
    require(
      flag.toInt != 0,
      "Incorrect flag for witness transaction, this must NOT be 0 according to BIP141, got: " + flag)
    val txInputBytes = bytes.slice(6, bytes.size)
    val (inputs, outputBytes) =
      BytesUtil.parseCmpctSizeUIntSeq(txInputBytes, TransactionInput)
    val (outputs, witnessBytes) =
      BytesUtil.parseCmpctSizeUIntSeq(outputBytes, TransactionOutput)
    val witness = TransactionWitness(witnessBytes, inputs.size)
    val lockTimeBytes = witnessBytes.drop(witness.byteSize)
    val lockTime = UInt32(lockTimeBytes.take(4).reverse)

    WitnessTransaction(version, inputs, outputs, lockTime, witness)
  }

  def toWitnessTx(tx: Transaction): WitnessTransaction =
    tx match {
      case btx: NonWitnessTransaction =>
        WitnessTransaction(btx.version,
                           btx.inputs,
                           btx.outputs,
                           btx.lockTime,
                           EmptyWitness.fromInputs(btx.inputs))
      case wtx: WitnessTransaction => wtx
    }

  val marker: Byte = 0.toByte
  val flag: Byte = 1.toByte

  /** These bytes -- at index 4 & 5 in a witness transaction -- are used to indicate a witness tx
    * @see BIP141 https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#transaction-id
    */
  val witBytes: ByteVector = ByteVector(marker, flag)
}
