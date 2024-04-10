package org.bitcoins.testkitcore.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkitcore.Implicits._
import org.scalacheck.Gen

import scala.annotation.tailrec

object TransactionGenerators {

  /** Responsible for generating [[org.bitcoins.core.protocol.transaction.TransactionOutPoint TransactionOutPoint]] */
  def outPoint: Gen[TransactionOutPoint] =
    for {
      txId <- CryptoGenerators.doubleSha256Digest
      vout <- NumberGenerator.uInt32s
    } yield TransactionOutPoint(txId, vout)

  /** Generates a random TX output paying to the given SPK */
  def outputTo(spk: ScriptPubKey): Gen[TransactionOutput] =
    for {
      satoshis <- CurrencyUnitGenerator.positiveRealistic
    } yield TransactionOutput(satoshis, spk)

  /** Generates a random [[org.bitcoins.core.protocol.transaction.TransactionOutput TransactionOutput]] */
  def output: Gen[TransactionOutput] =
    for {
      satoshis <- CurrencyUnitGenerator.satoshis
      (scriptPubKey, _) <- ScriptGenerators.scriptPubKey
    } yield TransactionOutput(satoshis, scriptPubKey)

  def outputs = Gen.listOf(output)

  /** Outputs that only have a positive amount of satoshis, techinically the bitcoin protocol allows you
    * to have negative value outputs
    */
  def realisticOutput: Gen[TransactionOutput] =
    CurrencyUnitGenerator.positiveRealistic.flatMap { amt =>
      ScriptGenerators.scriptPubKey.map(spk => TransactionOutput(amt, spk._1))
    }

  def realisticOutputs: Gen[Seq[TransactionOutput]] =
    Gen.choose(1, 5).flatMap(n => Gen.listOfN(n, realisticOutput))

  def realisticWitnessOutput: Gen[TransactionOutput] = {
    CurrencyUnitGenerator.positiveRealistic.flatMap { amt =>
      ScriptGenerators.witnessScriptPubKeyV0.map(spk =>
        TransactionOutput(amt, spk._1))
    }
  }

  def realisticWitnessOutputs: Gen[Seq[TransactionOutput]] = {
    Gen.choose(1, 5).flatMap(n => Gen.listOfN(n, realisticWitnessOutput))
  }

  /** Generates a small list of TX outputs paying to the given SPK */
  def smallOutputsTo(spk: ScriptPubKey): Gen[Seq[TransactionOutput]] =
    Gen.choose(1, 5).flatMap(i => Gen.listOfN(i, outputTo(spk)))

  /** Generates a small list of [[org.bitcoins.core.protocol.transaction.TransactionOutput TransactionOutput]] */
  def smallOutputs: Gen[Seq[TransactionOutput]] =
    Gen.choose(0, 5).flatMap(i => Gen.listOfN(i, output))

  private def genAmounts(totalAmount: CurrencyUnit): Seq[CurrencyUnit] = {
    val numOutputs = Gen.choose(0, 5).sampleSome
    @tailrec
    def loop(
        remaining: Int,
        remainingAmount: CurrencyUnit,
        accum: Seq[CurrencyUnit]): Seq[CurrencyUnit] = {
      if (remaining <= 0) {
        accum
      } else {
        val amt = Gen
          .choose(100L, remainingAmount.toBigDecimal.toLongExact)
          .map(n => Satoshis(n))
          .sampleSome
        loop(remaining - 1, remainingAmount - amt, amt +: accum)
      }
    }
    loop(numOutputs, totalAmount, Nil)
  }

  /** Creates a small sequence of outputs whose total sum is <= totalAmount */
  def smallOutputs(totalAmount: CurrencyUnit): Gen[Seq[TransactionOutput]] = {
    val amts = genAmounts(totalAmount)
    val spks = Gen.listOfN(amts.size, ScriptGenerators.scriptPubKey.map(_._1))
    spks.flatMap { s =>
      s.zip(amts).map { case (spk, amt) =>
        TransactionOutput(amt, spk)
      }
    }
  }

  /**  Creates a small sequence of outputs whose total sum is <= totalAmount
    * @return Sequence of outputs and corresponding Redeem Scripts
    */
  def smallP2SHOutputs(totalAmount: CurrencyUnit): Gen[
    Seq[(TransactionOutput, ScriptPubKey)]] = {
    val amts = genAmounts(totalAmount)
    val spks = Gen.listOfN(amts.size, ScriptGenerators.p2shScriptPubKey)
    spks.flatMap { s =>
      s.zip(amts).map { case ((spk, _, rs), amt) =>
        (TransactionOutput(amt, spk), rs)
      }
    }
  }

  def smallP2WSHOutputs(totalAmount: CurrencyUnit): Gen[
    Seq[(TransactionOutput, ScriptPubKey)]] = {
    val amts = genAmounts(totalAmount)
    val spks = Gen.listOfN(amts.size, ScriptGenerators.p2wshSPKV0)
    spks.flatMap { s =>
      s.zip(amts).map { case ((spk, _, rs), amt) =>
        (TransactionOutput(amt, spk), rs)
      }
    }
  }

  /** Generates a random [[org.bitcoins.core.protocol.transaction.TransactionInput TransactionInput]] */
  def input: Gen[TransactionInput] =
    for {
      outPoint <- outPoint
      scriptSig <- ScriptGenerators.scriptSignature
      sequenceNumber <- NumberGenerator.uInt32s
      randomNum <- Gen.choose(0, 10)
    } yield {
      if (randomNum == 0) {
        //gives us a coinbase input
        CoinbaseInput(scriptSig, sequenceNumber)
      } else TransactionInput(outPoint, scriptSig, sequenceNumber)
    }

  def inputs: Gen[List[TransactionInput]] = Gen.nonEmptyListOf(input)

  /** Generates a small list of [[org.bitcoins.core.protocol.transaction.TransactionInput TransactionInput]] */
  def smallInputs: Gen[Seq[TransactionInput]] =
    Gen.choose(1, 5).flatMap(i => Gen.listOfN(i, input))

  /** Generates a small non empty list of
    * [[org.bitcoins.core.protocol.transaction.TransactionInput TransactionInput]]
    */
  def smallInputsNonEmpty: Gen[Seq[TransactionInput]] =
    Gen.choose(1, 5).flatMap(i => Gen.listOfN(i, input))

  def outputReference: Gen[OutputReference] = {
    for {
      outPoint <- outPoint
      output <- output
    } yield OutputReference(outPoint, output)
  }

  def realisticOutputReference: Gen[OutputReference] = {
    for {
      outPoint <- outPoint
      output <- realisticOutput
    } yield OutputReference(outPoint, output)
  }

  /** Generates an arbitrary [[org.bitcoins.core.protocol.transaction.Transaction Transaction]]
    * This transaction's [[org.bitcoins.core.protocol.transaction.TransactionInput TransactionInput]]s
    * will not evaluate to true inside of the
    * [[org.bitcoins.core.script.interpreter.ScriptInterpreter ScriptInterpreter]]
    */
  def transactions: Gen[Seq[Transaction]] = Gen.listOf(transaction)

  /** Generates a small list of [[org.bitcoins.core.protocol.transaction.Transaction Transaction]] */
  def smallTransactions: Gen[Seq[Transaction]] =
    Gen.choose(0, 10).flatMap(i => Gen.listOfN(i, transaction))

  def nonEmptySmallTransactions: Gen[Seq[Transaction]] = {
    Gen.choose(1, 10).flatMap(i => Gen.listOfN(i, transaction))
  }

  def transaction: Gen[Transaction] =
    Gen.oneOf(baseTransaction, witnessTransaction)

  def realisticTransaction: Gen[Transaction] =
    Gen.oneOf(realisticBaseTransaction, realisiticWitnessTransaction)

  def realisticTransactionWitnessOut: Gen[Transaction] =
    Gen.oneOf(realisticBaseTransactionWitnessOut,
              realisiticWitnessTransactionWitnessOut)

  /** Generates a transaction where at least one output pays to the given SPK */
  def transactionTo(spk: ScriptPubKey): Gen[Transaction] =
    Gen.oneOf(baseTransactionTo(spk), witnessTransactionTo(spk))

  /** Generates a transaction with at least one output */
  def nonEmptyOutputTransaction: Gen[Transaction] =
    TransactionGenerators.transaction.suchThat(_.outputs.nonEmpty)

  def baseTransaction: Gen[BaseTransaction] =
    for {
      version <- NumberGenerator.int32s
      is <- smallInputs
      os <- smallOutputs
      lockTime <- NumberGenerator.uInt32s
    } yield BaseTransaction(version, is, os, lockTime)

  def realisticBaseTransaction: Gen[BaseTransaction] = {
    for {
      version <- NumberGenerator.int32s
      is <- smallInputs
      os <- realisticOutputs
      lockTime <- NumberGenerator.uInt32s
    } yield BaseTransaction(version, is, os, lockTime)
  }

  def realisticBaseTransactionWitnessOut: Gen[BaseTransaction] = {
    for {
      version <- NumberGenerator.int32s
      is <- smallInputs
      os <- realisticWitnessOutputs
      lockTime <- NumberGenerator.uInt32s
    } yield BaseTransaction(version, is, os, lockTime)
  }

  /** Generates a legacy transaction with at least one output paying to the given SPK */
  def baseTransactionTo(spk: ScriptPubKey): Gen[BaseTransaction] =
    for {
      version <- NumberGenerator.int32s
      is <- smallInputs
      os <- smallOutputsTo(spk)
      lockTime <- NumberGenerator.uInt32s
    } yield BaseTransaction(version, is, os, lockTime)

  /** To avoid duplicating logic */
  private def witnessTxHelper(
      outputs: Seq[TransactionOutput]): Gen[WitnessTransaction] = {
    for {
      version <- NumberGenerator.int32s
      //we cannot have zero witnesses on a WitnessTx
      //https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L276-L281
      is <- smallInputsNonEmpty
      lockTime <- NumberGenerator.uInt32s
      //we have to have atleast one NON `EmptyScriptWitness` for a tx to be a valid WitnessTransaction, otherwise we
      //revert to using the `BaseTransaction` serialization format
      //notice we use the old serialization format if all witnesses are empty
      //https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L276-L281
      witness <-
        WitnessGenerators
          .transactionWitness(is.size)
          .suchThat(_.witnesses.exists(_ != EmptyScriptWitness))
    } yield WitnessTransaction(version, is, outputs, lockTime, witness)
  }

  /** Generates a random [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] */
  def witnessTransaction: Gen[WitnessTransaction] =
    for {
      os <- smallOutputs
      tx <- witnessTxHelper(os)
    } yield tx

  def realisiticWitnessTransaction: Gen[WitnessTransaction] =
    for {
      os <- realisticOutputs
      tx <- witnessTxHelper(os)
    } yield tx

  def realisiticWitnessTransactionWitnessOut: Gen[WitnessTransaction] =
    for {
      os <- realisticWitnessOutputs
      tx <- witnessTxHelper(os)
    } yield tx

  /** Generates a SegWit TX where at least one output pays to the given SPK */
  def witnessTransactionTo(spk: ScriptPubKey): Gen[WitnessTransaction] = {
    for {
      os <- smallOutputsTo(spk)
      tx <- witnessTxHelper(os)
    } yield tx
  }

  /** Creates a [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]], then creates a
    * [[org.bitcoins.core.protocol.script.P2PKScriptPubKey P2PKScriptPubKey]] from that private key
    * Finally creates a  [[org.bitcoins.core.protocol.transaction.Transaction Transaction]]
    * that spends the [[org.bitcoins.core.protocol.script.P2PKScriptPubKey P2PKScriptPubKey]] correctly
    */
  def signedP2PKTransaction: Gen[(BaseTxSigComponent, ECPrivateKey)] =
    for {
      (signedScriptSig, scriptPubKey, privateKey) <-
        ScriptGenerators.signedP2PKScriptSignature
      (creditingTx, outputIndex) = buildCreditingTransaction(scriptPubKey)
      (signedTx, inputIndex) =
        buildSpendingTransaction(creditingTx, signedScriptSig, outputIndex)
      output = creditingTx.outputs(outputIndex.toInt)
      signedTxSignatureComponent = BaseTxSigComponent(
        signedTx,
        inputIndex,
        output,
        Policy.standardScriptVerifyFlags)
    } yield (signedTxSignatureComponent, privateKey)

  /** Creates a [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]], then creates a
    * [[org.bitcoins.core.protocol.script.P2PKScriptPubKey P2PKHScriptPubKey]] from that private key
    * Finally creates  [[org.bitcoins.core.protocol.transaction.Transaction Transaction]] that spends the
    * [[org.bitcoins.core.protocol.script.P2PKScriptPubKey P2PKHScriptPubKey]] correctly
    */
  def signedP2PKHTransaction: Gen[(BaseTxSigComponent, ECPrivateKey)] =
    for {
      (signedScriptSig, scriptPubKey, privateKey) <-
        ScriptGenerators.signedP2PKHScriptSignature
      (creditingTx, outputIndex) = buildCreditingTransaction(scriptPubKey)
      (signedTx, inputIndex) =
        buildSpendingTransaction(creditingTx, signedScriptSig, outputIndex)
      output = creditingTx.outputs(outputIndex.toInt)
      signedTxSignatureComponent = BaseTxSigComponent(
        signedTx,
        inputIndex,
        output,
        Policy.standardScriptVerifyFlags)
    } yield (signedTxSignatureComponent, privateKey)

  /** Creates a sequence of [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]], then creates a
    * [[org.bitcoins.core.protocol.script.MultiSignatureScriptPubKey MultiSignatureScriptPubKey]] from those
    * private keys. Finally creates a [[org.bitcoins.core.protocol.transaction.Transaction Transaction]] that
    * spends the [[org.bitcoins.core.protocol.script.MultiSignatureScriptPubKey MultiSignatureScriptPubKey]] correctly
    */
  def signedMultiSigTransaction: Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (signedScriptSig, scriptPubKey, privateKeys) <-
        ScriptGenerators.signedMultiSignatureScriptSignature
      (creditingTx, outputIndex) = buildCreditingTransaction(scriptPubKey)
      (signedTx, inputIndex) =
        buildSpendingTransaction(creditingTx, signedScriptSig, outputIndex)
      output = creditingTx.outputs(outputIndex.toInt)
      signedTxSignatureComponent = BaseTxSigComponent(
        signedTx,
        inputIndex,
        output,
        Policy.standardScriptVerifyFlags)
    } yield (signedTxSignatureComponent, privateKeys)

  /** Creates a transaction which contains a
    * [[org.bitcoins.core.protocol.script.P2SHScriptSignature P2SHScriptSignature]] that correctly spends a
    * [[org.bitcoins.core.protocol.script.P2SHScriptPubKey P2SHScriptPubKey]]
    */
  def signedP2SHTransaction: Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (signedScriptSig, scriptPubKey, privateKey) <-
        ScriptGenerators.signedP2SHScriptSignature
      (creditingTx, outputIndex) = buildCreditingTransaction(
        signedScriptSig.redeemScript)
      (signedTx, inputIndex) =
        buildSpendingTransaction(creditingTx, signedScriptSig, outputIndex)
      output = TransactionOutput(creditingTx.outputs(outputIndex.toInt).value,
                                 scriptPubKey)
      signedTxSignatureComponent = BaseTxSigComponent(
        signedTx,
        inputIndex,
        output,
        Policy.standardScriptVerifyFlags)
    } yield (signedTxSignatureComponent, privateKey)

  /** Generates a validly constructed CLTV transaction, which has a 50/50 chance of being spendable or unspendable. */
  def randomCLTVTransaction: Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] = {
    Gen.oneOf(unspendableCLTVTransaction, spendableCLTVTransaction)
  }

  /** Creates a [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]], then creates a
    * [[org.bitcoins.core.protocol.script.CLTVScriptPubKey CLTVScriptPubKey]] from that private key
    * Finally creates a [[org.bitcoins.core.protocol.transaction.Transaction Transaction]] that CANNNOT spend the
    * [[org.bitcoins.core.protocol.script.CLTVScriptPubKey CLTVScriptPubKey]] because the LockTime requirement
    * is not satisfied (i.e. the transaction's lockTime has not surpassed the CLTV value in the
    * [[org.bitcoins.core.protocol.script.CLTVScriptPubKey CLTVScriptPubKey]])
    *
    * @return
    */
  def unspendableCLTVTransaction: Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (cltvLockTime, txLockTime) <- unspendableCLTVValues
      sequence <- NumberGenerator.uInt32s.suchThat(n => n < UInt32.max)
      (scriptSig, scriptPubKey, privKeys) <-
        ScriptGenerators
          .signedCLTVScriptSignature(cltvLockTime, txLockTime, sequence)
      unspendable = lockTimeTxHelper(scriptSig,
                                     scriptPubKey,
                                     privKeys,
                                     sequence,
                                     Some(txLockTime))
    } yield unspendable

  /**  Creates a [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]], then creates a
    *  [[org.bitcoins.core.protocol.script.CLTVScriptPubKey CLTVScriptPubKey]] from that private key.
    *  Finally creates a [[org.bitcoins.core.protocol.transaction.Transaction Transaction]] that can successfully
    *  spend the [[org.bitcoins.core.protocol.script.CLTVScriptPubKey CLTVScriptPubKey]]
    */
  def spendableCLTVTransaction: Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (cltvLockTime, txLockTime) <- spendableCLTVValues
      sequence <- NumberGenerator.uInt32s.suchThat(n => n < UInt32.max)
      (scriptSig, scriptPubKey, privKeys) <-
        ScriptGenerators
          .signedCLTVScriptSignature(cltvLockTime, txLockTime, sequence)
      spendable = lockTimeTxHelper(scriptSig,
                                   scriptPubKey,
                                   privKeys,
                                   sequence,
                                   Some(txLockTime))
    } yield spendable

  /**  Creates a [[org.bitcoins.crypto.ECPrivateKey ECPrivateKey]], then creates a
    *  [[org.bitcoins.core.protocol.script.CSVScriptPubKey CSVScriptPubKey]] from that private key.
    *  Finally creates a [[org.bitcoins.core.protocol.transaction.Transaction Transaction]] that can
    *  successfully spend the [[org.bitcoins.core.protocol.script.CSVScriptPubKey CSVScriptPubKey]]
    */
  def spendableCSVTransaction: Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (csvScriptNum, sequence) <- spendableCSVValues
      tx <- csvTransaction(csvScriptNum, sequence)
    } yield tx

  /** Creates a CSV transaction that's timelock has not been met */
  def unspendableCSVTransaction: Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (csvScriptNum, sequence) <- unspendableCSVValues
      tx <- csvTransaction(csvScriptNum, sequence)
    } yield tx

  def csvTransaction(
      csvScriptNum: ScriptNumber,
      sequence: UInt32): Gen[(BaseTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (signedScriptSig, csvScriptPubKey, privateKeys) <-
        ScriptGenerators
          .signedCSVScriptSignature(csvScriptNum, sequence)
    } yield lockTimeTxHelper(signedScriptSig,
                             csvScriptPubKey,
                             privateKeys,
                             sequence,
                             None)

  /** Generates a [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] that has all of
    * it's inputs signed correctly
    */
  def signedP2WPKHTransaction: Gen[(WitnessTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (_, wBaseTxSigComponent, privKeys) <-
        WitnessGenerators.signedP2WPKHTransactionWitness
    } yield (wBaseTxSigComponent, privKeys)

  /** Generates a [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] that has an input
    * spends a raw P2WSH [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    */
  def signedP2WSHP2PKTransaction: Gen[
    (WitnessTxSigComponentRaw, Seq[ECPrivateKey])] =
    for {
      (_, wBaseTxSigComponent, privKeys) <-
        WitnessGenerators.signedP2WSHP2PKTransactionWitness
    } yield (wBaseTxSigComponent, privKeys)

  /** Generates a [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] that has an
    * input spends a raw P2WSH [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    */
  def signedP2WSHP2PKHTransaction: Gen[
    (WitnessTxSigComponentRaw, Seq[ECPrivateKey])] =
    for {
      (_, wBaseTxSigComponent, privKeys) <-
        WitnessGenerators.signedP2WSHP2PKHTransactionWitness
    } yield (wBaseTxSigComponent, privKeys)

  def signedP2WSHMultiSigTransaction: Gen[
    (WitnessTxSigComponentRaw, Seq[ECPrivateKey])] =
    for {
      (_, wBaseTxSigComponent, privKeys) <-
        WitnessGenerators.signedP2WSHMultiSigTransactionWitness
    } yield (wBaseTxSigComponent, privKeys)

  /** Creates a signed P2SH(P2WPKH) transaction */
  def signedP2SHP2WPKHTransaction: Gen[
    (WitnessTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (signedScriptSig, scriptPubKey, privKeys, witness, amount) <-
        ScriptGenerators.signedP2SHP2WPKHScriptSignature
      (creditingTx, outputIndex) =
        buildCreditingTransaction(signedScriptSig.redeemScript, amount)
      (signedTx, inputIndex) = buildSpendingTransaction(
        TransactionConstants.validLockVersion,
        creditingTx,
        signedScriptSig,
        outputIndex,
        witness)
      output = TransactionOutput(creditingTx.outputs(outputIndex.toInt).value,
                                 scriptPubKey)
      signedTxSignatureComponent = WitnessTxSigComponent(
        signedTx,
        inputIndex,
        output,
        PreviousOutputMap.empty,
        Policy.standardScriptVerifyFlags)
    } yield (signedTxSignatureComponent, privKeys)

  def signedP2WSHTransaction: Gen[
    (WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHP2PKTransaction,
              signedP2WSHP2PKHTransaction,
              signedP2WSHMultiSigTransaction)
  }

  /** Creates a signed P2SH(P2WSH) transaction */
  def signedP2SHP2WSHTransaction: Gen[
    (WitnessTxSigComponent, Seq[ECPrivateKey])] =
    for {
      (witness, wBaseTxSigComponent, privKeys) <-
        WitnessGenerators.signedP2WSHTransactionWitness
      p2shScriptPubKey = P2SHScriptPubKey(wBaseTxSigComponent.scriptPubKey)
      p2shScriptSig = P2SHScriptSignature(
        wBaseTxSigComponent.scriptPubKey.asInstanceOf[WitnessScriptPubKey])
      (creditingTx, outputIndex) = buildCreditingTransaction(
        p2shScriptSig.redeemScript,
        wBaseTxSigComponent.amount)
      sequence =
        wBaseTxSigComponent.transaction
          .inputs(wBaseTxSigComponent.inputIndex.toInt)
          .sequence
      locktime = wBaseTxSigComponent.transaction.lockTime
      (signedTx, inputIndex) = buildSpendingTransaction(
        TransactionConstants.validLockVersion,
        creditingTx,
        p2shScriptSig,
        outputIndex,
        locktime,
        sequence,
        witness)
      output = TransactionOutput(creditingTx.outputs(outputIndex.toInt).value,
                                 p2shScriptPubKey)
      signedTxSignatureComponent = WitnessTxSigComponent(
        signedTx,
        inputIndex,
        output,
        PreviousOutputMap.empty,
        Policy.standardScriptVerifyFlags)
    } yield (signedTxSignatureComponent, privKeys)

  /** Builds a spending transaction according to bitcoin core
    * @return the built spending transaction and the input index for the script signature
    */
  def buildSpendingTransaction(
      version: Int32,
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32,
      locktime: UInt32,
      sequence: UInt32): (Transaction, UInt32) = {
    val output = TransactionOutput(CurrencyUnits.zero, EmptyScriptPubKey)
    buildSpendingTransaction(version,
                             creditingTx,
                             scriptSignature,
                             outputIndex,
                             locktime,
                             sequence,
                             Seq(output))
  }

  def buildSpendingTransaction(
      version: Int32,
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32,
      locktime: UInt32,
      sequence: UInt32,
      outputs: Seq[TransactionOutput]): (Transaction, UInt32) = {
    val os = if (outputs.isEmpty) {
      Seq(TransactionOutput(CurrencyUnits.zero, EmptyScriptPubKey))
    } else {
      outputs
    }
    val outpoint = TransactionOutPoint(creditingTx.txId, outputIndex)
    val input = TransactionInput(outpoint, scriptSignature, sequence)
    val tx = BaseTransaction(version, Seq(input), os, locktime)
    (tx, UInt32.zero)
  }

  /** Builds a spending transaction according to bitcoin core with max sequence and a locktime of zero.
    * @return the built spending transaction and the input index for the script signature
    */
  def buildSpendingTransaction(
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32): (Transaction, UInt32) = {
    buildSpendingTransaction(TransactionConstants.version,
                             creditingTx,
                             scriptSignature,
                             outputIndex,
                             TransactionConstants.lockTime,
                             TransactionConstants.sequence)
  }

  /** Builds a spending [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] with the given parameters */
  def buildSpendingTransaction(
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32,
      locktime: UInt32,
      sequence: UInt32,
      witness: TransactionWitness): (WitnessTransaction, UInt32) = {
    buildSpendingTransaction(TransactionConstants.version,
                             creditingTx,
                             scriptSignature,
                             outputIndex,
                             locktime,
                             sequence,
                             witness)
  }

  def buildSpendingTransaction(
      version: Int32,
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32,
      locktime: UInt32,
      sequence: UInt32,
      witness: TransactionWitness): (WitnessTransaction, UInt32) = {

    val outputs = dummyOutputs
    buildSpendingTransaction(version,
                             creditingTx,
                             scriptSignature,
                             outputIndex,
                             locktime,
                             sequence,
                             witness,
                             outputs)
  }

  def dummyOutputs: Seq[TransactionOutput] =
    Seq(TransactionOutput(CurrencyUnits.zero, EmptyScriptPubKey))

  def buildSpendingTransaction(
      version: Int32,
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32,
      locktime: UInt32,
      sequence: UInt32,
      witness: TransactionWitness,
      outputs: Seq[TransactionOutput]): (WitnessTransaction, UInt32) = {
    val outpoint = TransactionOutPoint(creditingTx.txId, outputIndex)
    val input = TransactionInput(outpoint, scriptSignature, sequence)
    (WitnessTransaction(version, Seq(input), outputs, locktime, witness),
     UInt32.zero)
  }

  def buildSpendingTransaction(
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32,
      witness: TransactionWitness): (WitnessTransaction, UInt32) = {
    buildSpendingTransaction(TransactionConstants.version,
                             creditingTx,
                             scriptSignature,
                             outputIndex,
                             witness)
  }

  def buildSpendingTransaction(
      version: Int32,
      creditingTx: Transaction,
      scriptSignature: ScriptSignature,
      outputIndex: UInt32,
      witness: TransactionWitness): (WitnessTransaction, UInt32) = {
    val locktime = TransactionConstants.lockTime
    val sequence = TransactionConstants.sequence
    buildSpendingTransaction(version,
                             creditingTx,
                             scriptSignature,
                             outputIndex,
                             locktime,
                             sequence,
                             witness)
  }

  /** Mimics this test utility found in bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L57
    * @return the transaction and the output index of the scriptPubKey
    */
  def buildCreditingTransaction(
      scriptPubKey: ScriptPubKey): (Transaction, UInt32) = {
    //this needs to be all zeros according to these 3 lines in bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L64
    //https://github.com/bitcoin/bitcoin/blob/80d1f2e48364f05b2cdf44239b3a1faa0277e58e/src/primitives/transaction.h#L32
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/uint256.h#L40
    buildCreditingTransaction(TransactionConstants.version, scriptPubKey)
  }

  def buildCreditingTransaction(
      scriptPubKey: ScriptPubKey,
      amount: CurrencyUnit): (Transaction, UInt32) = {
    buildCreditingTransaction(TransactionConstants.version,
                              scriptPubKey,
                              amount)
  }

  /** Builds a crediting transaction with a transaction version parameter.
    * Example: useful for creating transactions with scripts containing OP_CHECKSEQUENCEVERIFY.
    * @return
    */
  def buildCreditingTransaction(
      version: Int32,
      scriptPubKey: ScriptPubKey): (Transaction, UInt32) = {
    buildCreditingTransaction(version, scriptPubKey, CurrencyUnits.zero)
  }

  def buildCreditingTransaction(
      version: Int32,
      output: TransactionOutput): (Transaction, UInt32) = {
    val outpoint = EmptyTransactionOutPoint
    buildCreditingTransaction(version, output, outpoint)
  }

  def buildCreditingTransaction(
      version: Int32,
      scriptPubKey: ScriptPubKey,
      amount: CurrencyUnit): (Transaction, UInt32) = {
    buildCreditingTransaction(version, TransactionOutput(amount, scriptPubKey))
  }

  def buildCreditingTransaction(
      version: Int32,
      output: TransactionOutput,
      outpoint: TransactionOutPoint): (Transaction, UInt32) = {
    val scriptSignature = ScriptSignature("0000")
    val input =
      TransactionInput(outpoint, scriptSignature, TransactionConstants.sequence)
    val tx = BaseTransaction(version,
                             Seq(input),
                             Seq(output),
                             TransactionConstants.lockTime)
    (tx, UInt32.zero)
  }

  private def lockTimeTxHelper(
      signedScriptSig: LockTimeScriptSignature,
      lock: LockTimeScriptPubKey,
      privKeys: Seq[ECPrivateKey],
      sequence: UInt32,
      lockTime: Option[UInt32]): (BaseTxSigComponent, Seq[ECPrivateKey]) = {
    val (creditingTx, outputIndex) =
      buildCreditingTransaction(TransactionConstants.validLockVersion, lock)
    //Transaction version must not be less than 2 for a CSV transaction
    val (signedSpendingTx, inputIndex) = buildSpendingTransaction(
      TransactionConstants.validLockVersion,
      creditingTx,
      signedScriptSig,
      outputIndex,
      lockTime.getOrElse(TransactionConstants.lockTime),
      sequence)
    val output = creditingTx.outputs(outputIndex.toInt)
    val baseTxSigComponent = BaseTxSigComponent(
      signedSpendingTx,
      inputIndex,
      output,
      Policy.standardScriptVerifyFlags)
    (baseTxSigComponent, privKeys)
  }

  /** Determines if the transaction input's sequence value and CSV script sequence value are of the same type
    * (i.e. determines whether both are a timestamp or block-height)
    */
  private def csvLockTimesOfSameType(
      sequenceNumbers: (ScriptNumber, UInt32)): Boolean = {
    LockTimeInterpreter.isCSVLockByRelativeLockTime(sequenceNumbers._1,
                                                    sequenceNumbers._2) ||
    LockTimeInterpreter.isCSVLockByBlockHeight(sequenceNumbers._1,
                                               sequenceNumbers._2)
  }

  /** Generates a pair of CSV values: a transaction input sequence, and a CSV script sequence value, such that the txInput
    * sequence mask is always greater than the script sequence mask (i.e. generates values for a validly constructed and spendable CSV transaction)
    */
  def spendableCSVValues: Gen[(ScriptNumber, UInt32)] = {
    Gen.oneOf(validScriptNumberAndSequenceForBlockHeight,
              validScriptNumberAndSequenceForRelativeLockTime)
  }

  /** To indicate that we should evaulate a [[org.bitcoins.core.script.locktime.OP_CHECKSEQUENCEVERIFY OP_CSV]]
    * operation based on
    * blockheight we need 1 << 22 bit turned off. See BIP68 for more details
    */
  private def lockByBlockHeightBitSet: UInt32 = UInt32("ffbfffff")

  /** Generates a [[org.bitcoins.core.number.UInt32 UInt32]] s.t. the block height bit is set according to BIP68 */
  private def sequenceForBlockHeight: Gen[UInt32] =
    validCSVSequence.map { n =>
      val result: UInt32 = n & lockByBlockHeightBitSet
      require(LockTimeInterpreter.isCSVLockByBlockHeight(result),
              "Block height locktime bit was not set: " + result)
      result
    }

  /** Generates a [[org.bitcoins.core.script.constant.ScriptNumber ScriptNumber]] and
    * [[org.bitcoins.core.number.UInt32 UInt32]] s.t. the pair can be spent by an OP_CSV operation
    */
  private def validScriptNumberAndSequenceForBlockHeight: Gen[
    (ScriptNumber, UInt32)] = {
    sequenceForBlockHeight.flatMap { s =>
      val seqMasked = TransactionConstants.sequenceLockTimeMask
      val validScriptNums = s & seqMasked
      Gen.choose(0L, validScriptNums.toLong).map { sn =>
        val scriptNum = ScriptNumber(sn & lockByBlockHeightBitSet.toLong)
        require(LockTimeInterpreter.isCSVLockByBlockHeight(scriptNum))
        require(LockTimeInterpreter.isCSVLockByBlockHeight(s))
        (scriptNum, s)
      }
    }
  }

  /** Generates a [[org.bitcoins.core.number.UInt32 UInt32]] with the locktime bit set according to BIP68 */
  private def sequenceForRelativeLockTime: Gen[UInt32] =
    validCSVSequence.map { n =>
      val result = n | TransactionConstants.sequenceLockTimeTypeFlag
      require(LockTimeInterpreter.isCSVLockByRelativeLockTime(result),
              "Relative locktime bit was not set: " + result)
      result
    }

  /** Generates a valid [[org.bitcoins.core.script.constant.ScriptNumber ScriptNumber]] and
    * [[org.bitcoins.core.number.UInt32 UInt32]] s.t. the pair will evaluate to true by a OP_CSV operation
    */
  private def validScriptNumberAndSequenceForRelativeLockTime: Gen[
    (ScriptNumber, UInt32)] = {
    sequenceForRelativeLockTime.flatMap { s =>
      val seqMasked = TransactionConstants.sequenceLockTimeMask
      val validScriptNums = s & seqMasked
      Gen.choose(0L, validScriptNums.toLong).map { sn =>
        val scriptNum = ScriptNumber(
          sn | TransactionConstants.sequenceLockTimeTypeFlag.toLong)
        require(LockTimeInterpreter.isCSVLockByRelativeLockTime(scriptNum))
        require(LockTimeInterpreter.isCSVLockByRelativeLockTime(s))
        (scriptNum, s)
      }
    }
  }

  /** Generates a [[org.bitcoins.core.number.UInt32 UInt32]] s.t. the locktime enabled flag is set.
    * See [[https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki BIP68]] for more info
    */
  private def validCSVSequence: Gen[UInt32] =
    NumberGenerator.uInt32s.map { n =>
      //makes sure the 1 << 31 is TURNED OFF,
      //need this to generate spendable CSV values without discarding a bunch of test cases
      val result = n & UInt32(0x7fffffff)
      require(LockTimeInterpreter.isLockTimeBitOff(ScriptNumber(result.toLong)))
      result
    }

  /** Generates a pair of CSV values: a transaction input sequence, and a CSV script sequence value, such that
    * the txInput sequence mask is always less than the script sequence mask (i.e. generates values for a validly
    * constructed and NOT spendable CSV transaction).
    */
  def unspendableCSVValues: Gen[(ScriptNumber, UInt32)] =
    (for {
      sequence <- NumberGenerator.uInt32s
      csvScriptNum <-
        NumberGenerator.uInt32s
          .map(x => ScriptNumber(x.toLong))
          .suchThat(x => LockTimeInterpreter.isLockTimeBitOff(x))
    } yield (csvScriptNum, sequence)).suchThat(x => !csvLockTimesOfSameType(x))

  /** generates a [[org.bitcoins.core.script.constant.ScriptNumber ScriptNumber]] and
    * [[org.bitcoins.core.number.UInt32 UInt32]] locktime for a transaction such that the tx will be spendable
    */
  def spendableCLTVValues: Gen[(ScriptNumber, UInt32)] =
    for {
      txLockTime <- NumberGenerator.uInt32s
      cltvLockTime <- sameLockTimeTypeSpendable(txLockTime)
    } yield (cltvLockTime, txLockTime)

  /** Generates a [[org.bitcoins.core.script.constant.ScriptNumber ScriptNumber]] and
    * [[org.bitcoins.core.number.UInt32 UInt32]] locktime for a transaction such that the tx will be unspendable
    */
  def unspendableCLTVValues: Gen[(ScriptNumber, UInt32)] =
    for {
      txLockTime <- NumberGenerator.uInt32s
      cltvLockTime <- sameLockTimeUnspendable(txLockTime)
    } yield (cltvLockTime, txLockTime)

  private def sameLockTimeTypeSpendable(
      txLockTime: UInt32): Gen[ScriptNumber] = {
    if (txLockTime < TransactionConstants.locktimeThreshold) {
      Gen.choose(0L, txLockTime.toLong).map(ScriptNumber(_))
    } else {
      Gen
        .choose(TransactionConstants.locktimeThreshold.toLong,
                txLockTime.toLong)
        .map(ScriptNumber(_))
    }
  }

  private def sameLockTimeUnspendable(txLockTime: UInt32): Gen[ScriptNumber] = {
    if (txLockTime < TransactionConstants.locktimeThreshold) {
      Gen
        .choose(txLockTime.toLong + 1,
                TransactionConstants.locktimeThreshold.toLong)
        .map(ScriptNumber(_))
    } else {
      Gen.choose(txLockTime.toLong + 1, UInt32.max.toLong).map(ScriptNumber(_))
    }
  }
}
