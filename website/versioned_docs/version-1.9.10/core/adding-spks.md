---
id: version-1.9.10-adding-spks
title: Adding New Script Types
original_id: adding-spks
---


<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
<!-- END doctoc -->

- [Adding a New ScriptPubKey Type](#adding-a-new-scriptpubkey-type)
  - [Step 0: Design Philosophy](#step-0-design-philosophy)
  - [Step 1: Create a New ScriptPubKey Trait](#step-1-create-a-new-scriptpubkey-trait)
  - [Step 2: Create Companion Object](#step-2-create-companion-object)
  - [Step 3: Add to Relevant fromAsm Methods](#step-3-add-to-relevant-fromasm-methods)
  - [Step 4: Create a ScriptSignature If Necessary](#step-4-create-a-scriptsignature-if-necessary)
  - [Step 5: Add to ScriptSignature.fromAsm If Applicable](#step-5-add-to-scriptsignaturefromasm-if-applicable)
  - [Step 6: Create Relevant BitcoinUTXOSpendingInfo](#step-6-create-relevant-bitcoinutxospendinginfo)
    - [Non-Nested Single-Key Spending Info](#non-nested-single-key-spending-info)
    - [Non-Nested Multi-Key Spending Info](#non-nested-multi-key-spending-info)
    - [Nested Spending Info](#nested-spending-info)
  - [Step 7: Add to Relevant Apply Methods](#step-7-add-to-relevant-apply-methods)
  - [Step 8: Create a Signer](#step-8-create-a-signer)
    - [Non-Nested Single-Key Spending Info](#non-nested-single-key-spending-info-1)
    - [Non-Nested Multi-Key Spending Info](#non-nested-multi-key-spending-info-1)
    - [Nested Spending Info](#nested-spending-info-1)
  - [Step 9: Add to BitcoinSigner.sign](#step-9-add-to-bitcoinsignersign)
  - [Step 10: Add to ScriptGenerators](#step-10-add-to-scriptgenerators)
    - [ScriptPubKey Generator](#scriptpubkey-generator)
    - [ScriptSignature Generator](#scriptsignature-generator)
    - [ScriptPubKey with Paired ScriptSignature Generator](#scriptpubkey-with-paired-scriptsignature-generator)
  - [Step 11: Add to CreditingTxGen](#step-11-add-to-creditingtxgen)
  - [Step 12: Fix all Non-Exhaustive Matches](#step-12-fix-all-non-exhaustive-matches)
  - [Step 13: Run tests and debug](#step-13-run-tests-and-debug)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Adding a New ScriptPubKey Type

In this document, we will describe how to add new script implementations and types in Bitcoin-S. We will use the following script template example which we have called P2PK with Timeout to illustrate the process:

```
OP_IF
    <Public Key>
OP_ELSE
    <Timeout> OP_CHECKSEQUENCEVERIFY OP_DROP
    <Timeout Public Key>
OP_ENDIF
OP_CHECKSIG
```

Here is [the actual pull request](https://github.com/bitcoin-s/bitcoin-s/pull/967) in which a very similar `ScriptPubKey` is implemented in Bitcoin-S.

Please note that this document only explains how to add new `RawScriptPubKey`s which are the subset of `ScriptPubKey`s which are fully described by their raw scripts. This is to say that this guide will not help in implementing a new segwit version, but should be helpful for most anything else.

It is also important to note that all new scripts should be implemented as if they are to appear on-chain without any P2SH or P2WSH. Bitcoin-S already supports conversions from raw on-chain scripts to these formats in the constructors for the script hash schemes which does not require extra support for new script types.

## Step 0: Design Philosophy

Bitcoin-S strives to have script types defined in such a way that they can be easily composed and reused. Before going through this guide and implementing a really large script template type, try to decompose your script into smaller re-usable pieces.

Also remember to consider what existing pieces you can use. For example, `LockTimeScriptPubKey`s are implemented in such a way that any other `RawScriptPubKey` can be given a time lock by nesting it within a `LockTimeScriptPubKey` subtype. Likewise, `ConditionalScriptPubKey`s are built to allow any other `RawScriptPubKey` type to populate both the `OP_IF/OP_NOTIF` case and the `OP_ELSE` case.

## Step 1: Create a New ScriptPubKey Trait

Go to `ScriptPubKey.scala` and add a new trait:

```scala
sealed trait P2PKWithTimeoutScriptPubKey extends RawScriptPubKey
```

You will then want to add all of the relevant accessor methods. For our case of P2PKWithTimeout, this will mean giving access to the public key, timeout, and timeout public key. Lastly, you will want to add a scaladoc. In total, we get the following result:

```scala
/** The type for ScriptPubKeys of the form:
  * OP_IF
  *   <Public Key>
  * OP_ELSE
  *   <Timeout> OP_CHECKSEQUENCEVERIFY OP_DROP
  *   <Timeout Public Key>
  * OP_ENDIF
  * OP_CHECKSIG
  */
sealed trait P2PKWithTimeoutScriptPubKey extends RawScriptPubKey {

  lazy val pubKey: ECPublicKey =
    ECPublicKey.fromBytes(this.asm(2).bytes)

  lazy val lockTime: ScriptNumber = ScriptNumber.fromBytes(this.asm(5).bytes)

  lazy val timeoutPubKey: ECPublicKey =
    ECPublicKey.fromBytes(this.asm(9).bytes)
}
```

## Step 2: Create Companion Object

We now need a companion object which will fulfill four functionalities for us:

1. Contain a concrete `Impl` class for our SPK type
   - This simply means creating a `private case class` wrapping `asm: Vector[ScriptToken]`
2. Create a `fromAsm` constructor
   - This should be a simple call to `buildScript` which is inherited from `ScriptFactory`
3. Create a logical constructor from Bitcoin-S types
   - This means creating an `apply` method that takes in logical BItcoin-S types and constructs asm
   - Note that this may require the use of `BitcoinScriptUtil.calculatePushOp`
4. Create an ASM filter which will detect if a given `Vector[ScriptToken]` corresponds to our type

This looks like the following:

```scala
object P2PKWithTimeoutScriptPubKey
    extends ScriptFactory[P2PKWithTimeoutScriptPubKey] {
  private case class P2PKWithTimeoutScriptPubKeyImpl(asm: Vector[ScriptToken])
      extends P2PKWithTimeoutScriptPubKey

  override def fromAsm(asm: Seq[ScriptToken]): P2PKWithTimeoutScriptPubKey = {
    buildScript(
      asm = asm.toVector,
      constructor = P2PKWithTimeoutScriptPubKeyImpl.apply,
      errorMsg = s"Given asm was not a P2PKWithTimeoutScriptPubKey, got $asm"
    )
  }

  def apply(
      pubKey: ECPublicKey,
      lockTime: ScriptNumber,
      timeoutPubKey: ECPublicKey): P2PKWithTimeoutScriptPubKey = {
    val timeoutAsm = CSVScriptPubKey(lockTime, EmptyScriptPubKey).asm.toVector
    val pubKeyAsm = BitcoinScriptUtil
      .calculatePushOp(pubKey.bytes)
      .toVector ++ Vector(ScriptConstant(pubKey.bytes))
    val timeoutPubKeyAsm = BitcoinScriptUtil
      .calculatePushOp(timeoutPubKey.bytes)
      .toVector ++ Vector(ScriptConstant(timeoutPubKey.bytes))

    P2PKWithTimeoutScriptPubKeyImpl(
      Vector(Vector(OP_IF),
             pubKeyAsm,
             Vector(OP_ELSE),
             timeoutAsm,
             timeoutPubKeyAsm,
             Vector(OP_ENDIF, OP_CHECKSIG)).flatten
    )
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    if (asm.length == 12) {
      val pubKey = ECPublicKey.fromBytes(asm(2).bytes)
      val lockTimeTry = Try(ScriptNumber.fromBytes(asm(5).bytes))
      val timeoutPubKey = ECPublicKey.fromBytes(asm(9).bytes)

      lockTimeTry match {
        case Success(lockTime) =>
          asm == P2PKWithTimeoutScriptPubKey(pubKey, lockTime, timeoutPubKey).asm
        case Failure(_) => false
      }
    } else {
      false
    }
  }
}
```

## Step 3: Add to Relevant fromAsm Methods

We now need to ensure that `ScriptPubKey.fromAsm(p2pkWithTimeoutSPK.asm)` returns our type. Since `P2PKWithTimeoutScriptPubKey extends RawScriptPubKey`, this means we must add to `RawScriptPubKey.fromAsm`. Note that order in this function's `match` can matter. Since our type is more specific than any other currently existing type, we put our new `case` at the top:

```scala
asm match {
    case Nil => EmptyScriptPubKey
    case _ if P2PKWithTimeoutScriptPubKey.isValidAsm(asm) =>
      P2PKWithTimeoutScriptPubKey.fromAsm(asm)
    //...
}
```

## Step 4: Create a ScriptSignature If Necessary

Often times a new `ScriptSignature` type will be necessary when introducing a new `ScriptPubKey` type. When this is the case, the procedure for adding a new `ScriptSignature` is more or less identical to steps 1 and 2 above. Here is what this looks like for `P2PKScriptPubKey` (note, this is not `P2PKWithTimeoutScriptPubKey`):

```scala
/**
  * Represents a pay to public key script signature
  * https://bitcoin.org/en/developer-guide#pubkey
  * Signature script: <sig>
  */
sealed trait P2PKScriptSignature extends ScriptSignature {

  /** PubKey scriptSignatures only have one signature */
  def signature: ECDigitalSignature = signatures.head

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(BitcoinScriptUtil.filterPushOps(this.asm).head.hex))
  }

  override def toString = s"P2PKScriptSignature($signature)"
}

object P2PKScriptSignature extends ScriptFactory[P2PKScriptSignature] {
  private case class P2PKScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends P2PKScriptSignature

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptSignature = {
    buildScript(asm.toVector,
                P2PKScriptSignatureImpl(_),
                "The given asm tokens were not a p2pk script sig: " + asm)
  }

  def apply(signature: ECDigitalSignature): P2PKScriptSignature = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val signatureConstant = ScriptConstant(signature.bytes)
    val asm = pushOps ++ Seq(signatureConstant)
    P2PKScriptSignature.fromAsm(asm)
  }

  /** P2PK scriptSigs always have the pattern [pushop, digitalSignature] */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = asm match {
    case Seq(_: BytesToPushOntoStack, _: ScriptConstant) => true
    case _                                               => false
  }
}
```

However, it is sometimes not necessary to create a new `ScriptSignature` type for every new `ScriptPubKey`. This is because we want to maintain unique representations for every `ScriptSignature`, and it turns out that in our case of `P2PKWithTimeoutScriptPubKey`, script signatures are of the form

```
<boolean> <signautre>
```

which is already represented by `ConditionalScriptSignature`. When this happens, you only need to create an `object` for your new type, and then follow step 2 above, skipping the first part (adding an Impl `case class`):

```scala
object P2PKWithTimeoutScriptSignature
    extends ScriptFactory[ConditionalScriptSignature] {
  override def fromAsm(asm: Seq[ScriptToken]): ConditionalScriptSignature = {
    buildScript(
      asm.toVector,
      ConditionalScriptSignature.fromAsm,
      s"The given asm tokens were not a P2PKWithTimeoutScriptSignature, got $asm"
    )
  }

  def apply(
      beforeTimeout: Boolean,
      signature: ECDigitalSignature): ConditionalScriptSignature = {
    ConditionalScriptSignature(P2PKScriptSignature(signature), beforeTimeout)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    P2PKScriptSignature.isValidAsm(asm.dropRight(1)) && ConditionalScriptSignature
      .isValidAsm(asm)
  }
}
```

Remember that in all of them above, `ScriptSignature`s are written as if they are to appear on-chain in transaction inputs rather than transaction witnesses, since Bitcoin-S supports turning any raw `ScriptSignature` into a `P2WSHWitness` without requiring explicit support for new script types.

## Step 5: Add to ScriptSignature.fromAsm If Applicable

If you added a new `ScriptSignature` type in the previous step, you must add a `case` to the `match` statement in `ScriptSignature.fromAsm` at the bottom of `ScriptSignature.scala`. For `P2PKScriptSignature` (note that this does not apply to `P2PKWithTimeoutScriptSignature` since there is no new unique type for this `ScriptSignature`), this looks like:

```scala
tokens match {
  //...
  case _ if P2PKScriptSignature.isValidAsm(tokens) =>
      P2PKScriptSignature.fromAsm(tokens)
  //...
}
```

## Step 6: Create Relevant InputInfo

`InputInfo` is the Bitcoin-S data structure for the information required to spend from a specific condition of a given `ScriptPubKey` other than private keys. Hence, when defining new script types, it is important to define how they are spent as well so that they can be useful.

There are two distinct kinds of scripts when it comes to signing in Bitcoin-S: scripts that have nesting (such as `ConditionalScriptPubKey`, `P2SHScriptPubKey`) and scripts without nesting (such as `MultiSignatureScriptPubKey`, `P2PKWithTimeoutScriptPubKey`, `P2PKHScriptPubKey`). We will cover each of these cases in turn, starting with the latter case as it applies to our example of `P2PKWithTimeout`. In both cases, please make sure to validate any parameter data using `require` statements when necessary, but make things correct by construction instead whenever possible. For example, if there is a redeem script and a `ScriptPubKey` which must wrap this redeem script, take as a parameter only the redeem script and construct the `ScriptPubKey` internally so that it is sure to be consistent.

### Non-Nesting Input Info

We create a new `case class` in `InputInfo.scala` which extends `RawInputInfo` and which contains in its parameters, all of the info required for spending a specific condition other than private keys and `HashType`. Here is what this looks like for `P2PKWithTimeout`:

```scala
case class P2PKWithTimeoutInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKWithTimeoutScriptPubKey,
    isBeforeTimeout: Boolean)
    extends RawInputInfo {
  override def conditionalPath: ConditionalPath = {
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
  }

  override def pubKeys: Vector[ECPublicKey] =
    Vector(scriptPubKey.pubKey, scriptPubKey.timeoutPubKey)
}
```

### Nested Spending Info

The one new thing in the nested case is that we must create a `val nestedSpendingInfo: RawInputInfo` and make sure to pass on `hashPreImages: Vector[NetworkElement]` to the nested `InputInfo`, and pull public keys from the `nestedInputInfo`. For the case of spending `LockTimeScriptPubKey`s, this looks like the following:

```scala
case class LockTimeInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    conditionalPath: ConditionalPath,
    hashPreImages: Vector[NetworkElement] = Vector.empty
) extends RawInputInfo {

  val nestedInputInfo: RawInputInfo = RawInputInfo(
    outPoint,
    amount,
    scriptPubKey.nestedScriptPubKey,
    conditionalPath,
    hashPreImages)

  override def pubKeys: Vector[ECPublicKey] = nestedInputInfo.pubKeys
}
```

## Step 7: Add to Relevant Apply Methods

Now that we have created our new `RawInputInfo`, we need to add them to the general-purpose input info constructors. This means adding a `case` to `RawInputInfo.apply` for your new `ScriptPubKey` type which constructs your relevant `RawInputInfo` from generic types (given as parameters in the `apply` methods). For `P2PKWithTimeout`, this looks like the following:

```scala
scriptPubKey match {
      //...
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
              conditionalPath.headOption match {
                case None =>
                  throw new IllegalArgumentException(
                    "ConditionalPath must be specified for P2PKWithTimeout")
                case Some(beforeTimeout) =>
                  P2PKWithTimeoutInputInfo(outPoint,
                                           amount,
                                           p2pkWithTimeout,
                                           beforeTimeout)
              }
      //...
    }
```

## Step 8: Create a Signer

We must now add signing functionality for our new script type within `Signer.scala`. This time, we have three different cases depending on your new script type.

### Non-Nested Single-Key Spending Info

For the non-nested case where only a single key is required, all we must do is create a new class which extends `RawSingleKeyBitcoinSigner` and implements `keyAndSigToScriptSig`. For `P2PKWithTimeout` this looks like the following:

```scala
sealed abstract class P2PKWithTimeoutSigner
    extends RawSingleKeyBitcoinSigner[P2PKWithTimeoutInputInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: UTXOInfo[P2PKWithTimeoutInputInfo]): ScriptSignature = {
    P2PKWithTimeoutScriptSignature(spendingInfo.inputInfo.isBeforeTimeout, sig)
  }
}

object P2PKWithTimeoutSigner extends P2PKWithTimeoutSigner
```

### Non-Nested Multi-Key Spending Info

In the non-nested case where multiple keys are required, we must create a new `Signer`, which requires implementing the `sign` function. For `MultiSignature` this looks like the following:

```scala
sealed abstract class MultiSigSigner extends Signer[MultiSignatureInputInfo] {

  override def sign(
      spendingInfo: UTXOSatisfyingInfo[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: UTXOSatisfyingInfo[MultiSignatureInputInfo])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val keysAndSigsF = spendingInfo.toSingles.map { spendingInfoSingle =>
      signSingle(spendingInfoSingle, unsignedTx)
    }

    val signaturesF = Future.sequence(keysAndSigsF).map(_.map(_.signature))

    val scriptSigF = signaturesF.map { sigs =>
      MultiSignatureScriptSignature(sigs)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

object MultiSigSigner extends MultiSigSigner
```

### Nested Spending Info

When signing for a nested script structure, we must create a new `Signer`. You will need to make a delegating call with the `nestedSpendingInfo` to `BitcoinSigner.sign`, but you may also need to do whatever else is needed with the nested result to construct a correct `ScriptSignature`. For `ConditionalScriptSignature`, this all looks like:

```scala
/** Delegates to get a ScriptSignature for the case being
  * spent and then adds an OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalSigner extends Signer[ConditionalInputInfo] {

  override def sign(
      spendingInfo: UTXOSatisfyingInfo[InputInfo],
      unsignedTx: Transaction,
      spendingInfoToSatisfy: UTXOSatisfyingInfo[ConditionalInputInfo])(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val nestedSpendingInfo = spendingInfoToSatisfy.copy(
      inputInfo = spendingInfoToSatisfy.inputInfo.nestedInputInfo)

    val missingOpSigComponentF = BitcoinSigner.sign(spendingInfo,
                                                    unsignedTx,
                                                    nestedSpendingInfo)

    val scriptSigF = missingOpSigComponentF.map { sigComponent =>
      ConditionalScriptSignature(sigComponent.scriptSignature,
                                 spendingInfoToSatisfy.inputInfo.condition)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}
object ConditionalSigner extends ConditionalSigner
```

## Step 9: Add to BitcoinSigner.sign

We must now add the new signing functionality from the previous step to the general-purpose signing functions by adding a new `case` for your new `InputInfo` type in the `match` within `BitcoinSigner.sign`. In the case of `P2PKWithTimeout`, this looks like:

```scala
spendingInfoToSatisfy.inputInfo match {
      //...
      case p2pKWithTimeout: P2PKWithTimeoutInputInfo =>
              P2PKWithTimeoutSigner.sign(spendingInfo,
                                         unsignedTx,
                                         spendingFrom(p2pKWithTimeout))
      //...
    }
```

We have now fully implemented the new script type! But have we done it correctly? We must now add the new script type to the Bitcoin-S test framework so that our scripts get added to existing Bitcoin-S property-based tests.

## Step 10: Add to ScriptGenerators

The first step to adding our new script type to Bitcoin-S property-based tests is creating generators for our new `ScriptPubKey` and `ScriptSignature` types in `ScriptGenerators.scala`.

It is important to note that in the current Bitcoin-S generator framework for `ScriptPubKey`s, all conditionals always spend only their `OP_TRUE` cases.

### ScriptPubKey Generator

Let's start by creating a generator for our `ScriptPubKey`, this generator should also return the private keys that were used to create the `ScriptPubKey`. To construct this `Gen`, you will likely need to use other generators for the internal structures in your script such as keys and lock times. For `P2PKWithTimeout` this looks like:

```scala
def p2pkWithTimeoutScriptPubKey: Gen[
    (P2PKWithTimeoutScriptPubKey, Seq[ECPrivateKey])] =
    for {
      privKey <- CryptoGenerators.privateKey
      timeoutPrivKey <- CryptoGenerators.privateKey
      lockTime <- NumberGenerator.timeLockScriptNumbers
    } yield {
      (P2PKWithTimeoutScriptPubKey(privKey.publicKey,
                                   lockTime,
                                   timeoutPrivKey.publicKey),
       Vector(privKey, timeoutPrivKey))
    }
```

Note that the private key used in the `OP_TRUE` case is the `head` of the `Seq[ECPrivateKey]` returned. This makes it possible for tests that only spend the `OP_TRUE` case to find the correct key, as it is expected to be the first one.

We must now add this `Gen` to all of the following `def`s in `ScriptGenerators.scala`: `randomNonP2SHScriptPubKey, scriptPubKey, nonWitnessScriptPubKey, nonConditionalRawScriptPubKey, rawScriptPubKey`, and if your `ScriptPubKey` has no lock times, you must also add the above `Gen` to `nonConditionalNonLocktimeRawScriptPubKey, nonLocktimeRawScriptPubKey` as well.

### ScriptSignature Generator

We must also create a generator for our `ScriptSignature` type, even if we did not introduce a new `ScriptSignature` type (in our example of `P2PKWithTimeout` we use a specific form of `ConditionalScriptSignature`). Once again you will likely need to use other existing generators. For `P2PKWithTimeoutScriptSignature`, this looks like:

```scala
def p2pkWithTimeoutScriptSignature: Gen[ConditionalScriptSignature] =
    for {
      privKey <- CryptoGenerators.privateKey
      hash <- CryptoGenerators.doubleSha256Digest
      hashType <- CryptoGenerators.hashType
      signature = ECDigitalSignature.fromBytes(
        privKey.sign(hash).bytes ++ ByteVector.fromByte(hashType.byte))
      beforeTimeout <- NumberGenerator.bool
    } yield P2PKWithTimeoutScriptSignature(beforeTimeout, signature)
```

We now add this `Gen` to `scriptSignature: Gen[ScriptSignature]` as well as adding a case for our new `ScriptPubKey` type in `pickCorrespondingScriptSignature` which should return our new `ScriptSignature` generator.  If our `ScriptPubKey` does not have any lock times, you should also add this script signature `Gen` to `nonLockTimeConditionalScriptSignature` and `randomNonLockTimeScriptSig`.

### ScriptPubKey with Paired ScriptSignature Generator

Lastly, we need to construct a generator that returns both a `ScriptPubKey` and a `ScriptSignature` signing that that `ScriptPubKey`. All keys used in signing should also be returned. This all should be done by using the above `ScriptPubKey` generator, then constructing an `ScriptSignature` for your type where all actual signatures are `EmptyDigitalSignature`s. A `UTXOSatisfyingInfo` should then be constructed for the generated `ScriptPubKey` (using the private keys generated in the same line). Finally, a `TxSignatureComponent` should be created by using the new `Signer` for our script type. From this `TxSignatureComponent`, a `ScriptSignature` is readily available. For `P2PKWithTimeout`, this generator looks like:

```scala
def signedP2PKWithTimeoutScriptSignature: Gen[
    (ConditionalScriptSignature, P2PKWithTimeoutScriptPubKey, ECPrivateKey)] =
    for {
      (spk, privKeys) <- p2pkWithTimeoutScriptPubKey
      hashType <- CryptoGenerators.hashType
    } yield {
      val emptyScriptSig = P2PKWithTimeoutScriptSignature(beforeTimeout = true,
                                                          ECDigitalSignature.empty)
      val (creditingTx, outputIndex) =
        TransactionGenerators.buildCreditingTransaction(spk)
      val (spendingTx, inputIndex) = TransactionGenerators
        .buildSpendingTransaction(creditingTx, emptyScriptSig, outputIndex)
      val spendingInfo = UTXOSatisfyingInfo(
        P2PKWithTimeoutInputInfo(
          TransactionOutPoint(creditingTx.txIdBE, inputIndex),
          creditingTx.outputs(outputIndex.toInt).value,
          spk,
          isBeforeTimeout = true),
        privKeys.toVector,
        hashType
      )
      val txSigComponentF = P2PKWithTimeoutSigner.sign(spendingInfo,
                                                       spendingTx)
      val txSigComponent = Await.result(txSigComponentF, timeout)
      val signedScriptSig =
        txSigComponent.scriptSignature.asInstanceOf[ConditionalScriptSignature]

      (signedScriptSig, spk, privKeys.head)
    }
```

I strongly advise you also look at at least one other `Gen` of this kind before writing your own.

## Step 11: Add to CreditingTxGen

Now that we have generators constructed for `ScriptPubKey`s, `ScriptSignature`s and their pairings completed, we will create a generator for our type's `SpendingInfoFull`. This should usually be as simple as mapping on the `ScriptPubKey` generator in `ScriptGenerators` and calling `build` (within `CreditinTxGen.scala`). We then also create another generator which returns lists of `SpendingInfo`s generated by the previous `Gen`. For `P2PKWithTimeout`, this looks like:

```scala
def p2pkWithTimeoutOutput: Gen[UTXOSatisfyingInfo[InputInfo]] = {
    ScriptGenerators.p2pkWithTimeoutScriptPubKey.flatMap { p2pkWithTimeout =>
      build(p2pkWithTimeout._1, Seq(p2pkWithTimeout._2.head), None, None)
    }
  }

  def p2pkWithTimeoutOutputs: Gen[Seq[UTXOSatisfyingInfo[InputInfo]]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkWithTimeoutOutput))
  }
```

We must then add our output `Gen` to one of `cltvOutputGens` or `nonCLTVOutputGens` depending on whether the `ScriptPubKey` type has CLTVs (absolute lock times) or not. We must also add our output `Gen` to `nonP2SHOutput`, and also to `nonSHOutput` and `nonP2WSHOutput` in the case that your `ScriptPubKey` type has no CLTVs.

## Step 12: Fix all Non-Exhaustive Matches

All we have left is to clean up our code and make sure that nothing has been missed. Within an `sbt` terminal, you should run the following sequence of commands:

```bashrc
clean
project coreTest
test:compile
```

This should have quite a lengthy output but we are only interested in any compiler errors there may be, as well as non-exhaustive match compiler warnings. You should first fix any compiler errors you encounter, and then you can get the warnings again by running `clean` and then running `test:compile` again.

The warnings we're interested in should look something like this:

```
[warn] /home/nkohen/Desktop/SuredBits/bitcoin-s-core/testkit/src/main/scala/org/bitcoins/testkit/core/gen/ScriptGenerators.scala:524:59: match may not be exhaustive.
[warn] It would fail on the following input: P2PKWithTimeoutScriptPubKeyImpl(_)
[warn]       scriptPubKey: ScriptPubKey): Gen[ScriptSignature] = scriptPubKey match {
[warn]                                                           ^
[warn] one warning found
```

You may get these warnings for your new `ScriptSignature` type as well. These are places where the compiler expects there to be defined functionality in a pattern match where one of our new types is a possibility, but for which no functionality is defined. You must go to each of these warnings and add a `case` for the relevant new type, or add this new type to an existing case when applicable.

## Step 13: Run tests and debug

Lastly, once everything is compiling nicely, all that is left is to run tests and debug. While within an `sbt` terminal session, run the following two commands to run the relevant tests:

```
project coreTest
test
```

If all tests pass we are all done! If you encounter any test failures, you can re-run individual tests using the `testOnly` command which must be given the full name of the test you wish to run (these names should be at the bottom of the testing output and look something like `org.bitcoins.core.script.interpreter.ScriptInterpreterTest`).
