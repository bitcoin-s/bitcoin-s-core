---
id: version-1.9.10-psbts
title: Partially Signed Bitcoin Transactions
original_id: psbts
---
Creating unsigned or partially signed transactions to be passed 
around to other signers can be a useful for many applications.
PSBTs offer a standardized format to serialize the necessary data
for signing the transaction, as well as, validating that you in fact 
want to sign this transaction.
> If you want to jump into the details of the specification,
> you should checkout
> [BIP 174](https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki).

Bitcoin-S fully supports PSBTs with functionality for
creation, updating, combining, signing, finalizing, 
and transaction extraction.

An example on a typical PSBT workflow:


```scala
implicit val ec: ExecutionContextExecutor = ExecutionContext.global

// First you need an unsigned transaction,
// here we have a standard 2 input, 2 output transaction
// This transaction must be of type BaseTransaction
// and have empty ScriptSignatures for all of it's inputs
val unsignedTransaction = BaseTransaction(
    "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000")

// To create the initial PSBT all we need to do is
val emptyPSBT = PSBT.fromUnsignedTx(unsignedTransaction)

// Now that we have an empty PSBT we can start updating it with data we know

// First, we want to fill the UTXO fields that we will need for signing and extraction
// The transactions we add are the fully serialized transaction that we are spending from
val utxo0 = Transaction(
    "0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f618765000000")

val utxo1 = Transaction(
    "0200000000010158e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd7501000000171600145f275f436b09a8cc9a2eb2a2f528485c68a56323feffffff02d8231f1b0100000017a914aed962d6654f9a2b36608eb9d64d2b260db4f1118700c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e88702483045022100a22edcc6e5bc511af4cc4ae0de0fcd75c7e04d8c1c3a8aa9d820ed4b967384ec02200642963597b9b1bc22c75e9f3e117284a962188bf5e8a74c895089046a20ad770121035509a48eb623e10aace8bfd0212fdb8a8e5af3c94b0b133b95e114cab89e4f7965000000")

val psbtWithUTXOs = emptyPSBT.addUTXOToInput(utxo0, index = 0).addUTXOToInput(utxo1, index = 1)
// After we have the relevant UTXOs we can add the
// redeem scripts, witness scripts, and BIP 32 derivation paths if needed

// In this transaction the first input is a P2SH 2-of-2 multisig
// so we need to add its corresponding redeem script.
// Here we are just using a deserialized version of the redeem script but
// you may generate your ScriptPubKey another way in practice

val redeemScript0 = ScriptPubKey.fromAsmBytes(
    hex"5221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae")

val psbtWithUpdatedFirstInput =
    psbtWithUTXOs.addRedeemOrWitnessScriptToInput(redeemScript0, index = 0)

// The second input in this transaction is a P2SH(P2WSH) 2-of-2 multisig
// so we need to add its corresponding redeem script and witness script.
// Here we add them both using the same function, the PSBT updater will
// be able to figure out, based on the available data, where to correctly

val redeemScript1 = ScriptPubKey.fromAsmBytes(
    hex"00208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903")

val witnessScript = ScriptPubKey.fromAsmBytes(
    hex"522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae")

// put the data in the PSBT
val psbtWithUpdatedSecondInput = psbtWithUpdatedFirstInput
    .addRedeemOrWitnessScriptToInput(redeemScript1, index = 1)
    .addRedeemOrWitnessScriptToInput(witnessScript, index = 1)

// Before signing we need to add the needed SigHash flags so we know how to sign the transaction
// If one is not provided it will be assumed to be SigHashAll

val psbtWithSigHashFlags = psbtWithUpdatedSecondInput
    .addSigHashTypeToInput(HashType.sigHashAll, index = 0)
    .addSigHashTypeToInput(HashType.sigHashAll, index = 1)

// Next, we can now sign the PSBT
// Signing a PSBT will return a Future[PSBT] so this will need to be handled
// correctly in an application
// Here we use the relevant private keys to sign the first input
val privKey0 = ECPrivateKeyUtil.fromWIFToPrivateKey(
    "cP53pDbR5WtAD8dYAW9hhTjuvvTVaEiQBdrz9XPrgLBeRFiyCbQr").toPrivateKey

val privKey1 = ECPrivateKeyUtil.fromWIFToPrivateKey(
    "cR6SXDoyfQrcp4piaiHE97Rsgta9mNhGTen9XeonVgwsh4iSgw6d").toPrivateKey

val psbtFirstSig =
    psbtWithSigHashFlags
      .sign(inputIndex = 0, signer = privKey0)
      .sign(inputIndex = 0, signer = privKey1)

// Alternatively, you can use produce a signature with a ECSignatureParams
// using the BitcoinSigner will return a PartialSignature that can be added to a PSBT

// First we need to declare out spendingInfoSingle
val outPoint = unsignedTransaction.inputs.head.previousOutput
val output = utxo0.outputs(outPoint.vout.toInt)

val spendingInfoSingle = ECSignatureParams(
    InputInfo(outPoint = outPoint,
              output = output,
              redeemScriptOpt = Some(redeemScript0),
              scriptWitnessOpt = None,
              conditionalPath = ConditionalPath.NoCondition,
              previousOutputMap = PreviousOutputMap.empty),
    prevTransaction = utxo0,
    signer = privKey0,
    hashType = HashType.sigHashAll
  )

// Then we can sign the transaction
val signature = BitcoinSigner.signSingle(
    spendingInfo = spendingInfoSingle,
    unsignedTx = unsignedTransaction,
    privKey0.signLowRWithHashType)

// We can then add the signature to the PSBT
// Note: this signature could be produced by us or another party
psbtWithSigHashFlags.addSignature(signature, inputIndex = 0)

// With our first input signed we can now move on to showing how another party could sign our second input
// In this scenario, let's say that the second input does not belong to us and we need
// another party to sign it. In this case we would need to send the PSBT to the other party.
// The two standard formats for this are in byte form or in base64 you can access these easily.
val bytes = psbtFirstSig.bytes
val base64 = psbtFirstSig.base64

// After the other party has signed their input they can send us back the PSBT with the signatures
// To import we can use any of these functions
val fromBytes = PSBT.fromBytes(
  hex"70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f618765000000220202dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d7483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8872202023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e73473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d2010103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

val fromBase64 = PSBT.fromBase64(
  "cHNidP8BAJoCAAAAAljoeiG1ba8MI76OcHBFbDNvfLqlyHV5JPVFiHuyq911AAAAAAD")

// After we've imported the PSBT we can combine it with our own signed PSBT so we can
// have one PSBT with all of the necessary data
val combinedPSBT = fromBase64.combinePSBT(psbtFirstSig)

// Now that the PSBT has all the necessary data, we can finalize it and extract the transaction
// This will return a Try[PSBT] and will fail if you do not have all the required fields filled
val finalizedPSBT = combinedPSBT.finalizePSBT

// After it has been finalized we can extract the fully signed transaction that is ready
// to be broadcast to the network.
// You can also use extractTransactionAndValidate that will validate if the transaction is valid
finalizedPSBT.get.extractTransaction
```
