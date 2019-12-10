package org.bitcoins.dlc

import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2PKHScriptPubKey}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, CryptoUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerByte}
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  P2PKHSpendingInfo
}
import org.bitcoins.testkit.core.gen.{
  CurrencyUnitGenerator,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.Future

class BinaryOutcomeDLCWithSelfTest extends BitcoinSAsyncTest {
  behavior of "BinaryOutcomeDLCWithSelf"

  // Can't use TransactionGenerators.realisiticOutputs as that can return List.empty
  val nonEmptyRealisticOutputsGen: Gen[List[TransactionOutput]] = Gen
    .choose(1, 5)
    .flatMap(n => Gen.listOfN(n, TransactionGenerators.realisticOutput))
    .suchThat(_.nonEmpty)

  it should "correctly subtract fees evenly amongst outputs" in {
    forAllAsync(nonEmptyRealisticOutputsGen, CurrencyUnitGenerator.smallFeeUnit) {
      case (outputs, feeRate) =>
        val totalInput = outputs.foldLeft(CurrencyUnits.zero) {
          case (accum, output) =>
            accum + output.value
        }

        val inputKey = ECPrivateKey.freshPrivateKey
        val utxos: Vector[BitcoinUTXOSpendingInfo] = Vector(
          P2PKHSpendingInfo(
            outPoint =
              TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
            amount = totalInput,
            scriptPubKey = P2PKHScriptPubKey(inputKey.publicKey),
            signer = inputKey,
            hashType = HashType.sigHashAll
          ))
        val changeSPK = EmptyScriptPubKey
        val network: BitcoinNetwork = RegTest

        val txBuilderF =
          BitcoinTxBuilder(outputs, utxos, feeRate, changeSPK, network)

        val badFeeF = txBuilderF.flatMap { txBuilder =>
          recoverToSucceededIf[IllegalArgumentException](txBuilder.sign)
        }

        for {
          txBuilder <- txBuilderF
          _ <- badFeeF
          tx <- BinaryOutcomeDLCWithSelf.subtractFeeEqualAndSign(txBuilder)
        } yield {
          val diffs = outputs.zip(tx.outputs).map {
            case (before, after) =>
              before.value - after.value
          }

          val firstDiff = diffs.head
          // Fee has been evenly distributed (up to some remainder)
          assert(diffs.forall(diff =>
            diff - firstDiff < Satoshis(Int64(diffs.length))))
        }
    }
  }

  def closeTogether(num1: Double, num2: Double): Boolean = {
    Math.abs(num1 - num2) < 1.0e-6
  }

  it should "correctly subtract fees proportionally amongst outputs" in {
    forAllAsync(nonEmptyRealisticOutputsGen, CurrencyUnitGenerator.smallFeeUnit) {
      case (outputs, feeRate) =>
        val totalInput = outputs.foldLeft(CurrencyUnits.zero) {
          case (accum, output) =>
            accum + output.value
        }

        val inputKey = ECPrivateKey.freshPrivateKey
        val utxos: Vector[BitcoinUTXOSpendingInfo] = Vector(
          P2PKHSpendingInfo(
            outPoint =
              TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
            amount = totalInput,
            scriptPubKey = P2PKHScriptPubKey(inputKey.publicKey),
            signer = inputKey,
            hashType = HashType.sigHashAll
          ))
        val changeSPK = EmptyScriptPubKey
        val network: BitcoinNetwork = RegTest

        val txBuilderF =
          BitcoinTxBuilder(outputs, utxos, feeRate, changeSPK, network)

        val badFeeF = txBuilderF.flatMap { txBuilder =>
          recoverToSucceededIf[IllegalArgumentException](txBuilder.sign)
        }

        for {
          txBuilder <- txBuilderF
          _ <- badFeeF
          tx <- BinaryOutcomeDLCWithSelf.subtractFeeProportionalAndSign(
            txBuilder)
        } yield {
          val sizeAndDiffs = outputs.zip(tx.outputs).map {
            case (before, after) =>
              (before.value, before.value - after.value)
          }

          val (firstSize, firstFee) = sizeAndDiffs.head
          val firstFeeProportion = firstFee.satoshis.toLong.toDouble / firstSize.satoshis.toLong
          // Fee has been proportionally distributed (up to some remainder)
          val closeProportions = sizeAndDiffs.forall {
            case (size, fee) =>
              val feeProportion = fee.satoshis.toLong.toDouble / size.satoshis.toLong

              closeTogether(feeProportion / firstFeeProportion, 1)
          }

          assert(closeProportions)
        }
    }
  }

  val outcomeWin = "WIN"

  val outcomeWinHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
  val outcomeLose = "LOSE"

  val outcomeLoseHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip
  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: ECPublicKey = oraclePrivKey.publicKey
  val preCommittedK: SchnorrNonce = SchnorrNonce.freshNonce
  val preCommittedR: ECPublicKey = preCommittedK.publicKey
  val localInput: CurrencyUnit = CurrencyUnits.oneBTC
  val remoteInput: CurrencyUnit = CurrencyUnits.oneBTC

  val inputPrivKeyLocal: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal: ECPublicKey = inputPrivKeyLocal.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey

  val blockTimeToday: BlockTime = BlockTime(
    UInt32(System.currentTimeMillis() / 1000))

  val localFundingUtxos = Vector(
    P2PKHSpendingInfo(
      outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      amount = localInput,
      scriptPubKey = P2PKHScriptPubKey(inputPubKeyLocal),
      signer = inputPrivKeyLocal,
      hashType = HashType.sigHashAll
    )
  )

  val remoteFundingUtxos = Vector(
    P2PKHSpendingInfo(
      outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
      amount = remoteInput,
      scriptPubKey = P2PKHScriptPubKey(inputPubKeyRemote),
      signer = inputPrivKeyRemote,
      hashType = HashType.sigHashAll
    )
  )

  val changePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val changePubKey: ECPublicKey = changePrivKey.publicKey
  val changeSPK: P2PKHScriptPubKey = P2PKHScriptPubKey(changePubKey)

  val dlc: BinaryOutcomeDLCWithSelf = BinaryOutcomeDLCWithSelf(
    outcomeWin = outcomeWin,
    outcomeLose = outcomeLose,
    oraclePubKey = oraclePubKey,
    preCommittedR = preCommittedR,
    localExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv),
    remoteExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv),
    localInput = localInput,
    remoteInput = remoteInput,
    localFundingUtxos = localFundingUtxos,
    remoteFundingUtxos = remoteFundingUtxos,
    localWinPayout = localInput + CurrencyUnits.oneMBTC,
    localLosePayout = localInput - CurrencyUnits.oneMBTC,
    timeout = blockTimeToday,
    feeRate = SatoshisPerByte(Satoshis.one),
    changeSPK = changeSPK,
    network = RegTest
  )

  def validateOutcome(outcome: DLCOutcome): Assertion = {
    val DLCOutcome(fundingTx,
                   cet,
                   localClosingTx,
                   remoteClosingTx,
                   initialSpendingInfos,
                   fundingSpendingInfo,
                   localCetSpendingInfo,
                   remoteCetSpendingInfo) = outcome

    assert(
      BitcoinScriptUtil.verifyScript(fundingTx, initialSpendingInfos)
    )
    assert(
      BitcoinScriptUtil.verifyScript(cet, Vector(fundingSpendingInfo))
    )
    assert(
      BitcoinScriptUtil.verifyScript(localClosingTx,
                                     Vector(localCetSpendingInfo))
    )
    assert(
      BitcoinScriptUtil.verifyScript(remoteClosingTx,
                                     Vector(remoteCetSpendingInfo))
    )
  }

  def executeUnilateralForCase(
      outcomeHash: Sha256DigestBE,
      local: Boolean): Future[Assertion] = {
    val oracleSig =
      Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

    dlc.setupDLC().flatMap { setup =>
      dlc
        .executeUnilateralDLC(setup, Future.successful(oracleSig), local)
        .map(validateOutcome)
    }
  }

  def executeRefundCase(): Future[Assertion] = {
    val outcomeF = dlc.setupDLC().flatMap { setup =>
      dlc.executeRefundDLC(setup)
    }

    outcomeF.map(validateOutcome)
  }

  def executeJusticeCase(
      fakeWin: Boolean,
      local: Boolean): Future[Assertion] = {
    dlc.setupDLC().flatMap { setup =>
      val timedOutCET = if (fakeWin) {
        if (local) {
          setup.cetWinRemote
        } else {
          setup.cetWinLocal
        }
      } else {
        if (local) {
          setup.cetLoseRemote
        } else {
          setup.cetLoseLocal
        }
      }
      val outcomeF = dlc.executeJusticeDLC(setup, timedOutCET, local)

      outcomeF.map(validateOutcome)
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal win case" in {
    for {
      _ <- executeUnilateralForCase(outcomeWinHash, local = true)
      _ <- executeUnilateralForCase(outcomeWinHash, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal lose case" in {
    for {
      _ <- executeUnilateralForCase(outcomeLoseHash, local = true)
      _ <- executeUnilateralForCase(outcomeLoseHash, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    executeRefundCase()
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice win case" in {
    for {
      _ <- executeJusticeCase(fakeWin = true, local = true)
      _ <- executeJusticeCase(fakeWin = true, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice lose case" in {
    for {
      _ <- executeJusticeCase(fakeWin = false, local = true)
      _ <- executeJusticeCase(fakeWin = false, local = false)
    } yield succeed
  }
}
