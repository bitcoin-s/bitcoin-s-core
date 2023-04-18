package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.CoinSelectionAlgo._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.FeeUnit

import scala.annotation.tailrec
import scala.util.{Random, Try}
import org.bitcoins.core.currency.currencyUnitOrdering

/** Implements algorithms for selecting from a UTXO set to spend to an output set at a given fee rate. */
trait CoinSelector {

  /** Randomly selects utxos until it has enough to fund the desired amount,
    * should only be used for research purposes
    */
  def randomSelection(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val randomUtxos = Random.shuffle(walletUtxos)

    accumulate(randomUtxos, outputs, feeRate)
  }

  /** Greedily selects from walletUtxos starting with the largest outputs, skipping outputs with values
    * below their fees. Better for high fee environments than accumulateSmallestViable.
    */
  def accumulateLargest(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val sortedUtxos =
      walletUtxos.sortBy(_.prevOut.value).reverse

    accumulate(sortedUtxos, outputs, feeRate)
  }

  /** Greedily selects from walletUtxos starting with the smallest outputs, skipping outputs with values
    * below their fees. Good for low fee environments to consolidate UTXOs.
    *
    * Has the potential privacy breach of connecting a ton of UTXOs to one address.
    */
  def accumulateSmallestViable(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val sortedUtxos = walletUtxos.sortBy(_.prevOut.value)

    accumulate(sortedUtxos, outputs, feeRate)
  }

  /** Greedily selects from walletUtxos in order, skipping outputs with values below their fees */
  def accumulate(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val totalValue = outputs.foldLeft(CurrencyUnits.zero) {
      case (totVal, output) => totVal + output.value
    }

    @tailrec
    def addUtxos(
        alreadyAdded: Vector[CoinSelectorUtxo],
        valueSoFar: CurrencyUnit,
        bytesSoFar: Long,
        utxosLeft: Vector[CoinSelectorUtxo]): Vector[CoinSelectorUtxo] = {
      val fee = feeRate * bytesSoFar
      if (valueSoFar > totalValue + fee) {
        alreadyAdded
      } else if (utxosLeft.isEmpty) {
        throw new RuntimeException(
          s"Not enough value in given outputs ($valueSoFar) to make transaction spending $totalValue plus fees $fee")
      } else {
        val nextUtxo = utxosLeft.head
        val effectiveValue = calcEffectiveValue(nextUtxo, feeRate)
        if (effectiveValue <= Satoshis.zero) {
          addUtxos(alreadyAdded, valueSoFar, bytesSoFar, utxosLeft.tail)
        } else {
          val newAdded = alreadyAdded.:+(nextUtxo)
          val newValue = valueSoFar + nextUtxo.prevOut.value
          val approxUtxoSize = CoinSelector.approximateUtxoSize(nextUtxo)

          addUtxos(newAdded,
                   newValue,
                   bytesSoFar + approxUtxoSize,
                   utxosLeft.tail)
        }
      }
    }

    addUtxos(Vector.empty, CurrencyUnits.zero, bytesSoFar = 0L, walletUtxos)
  }

  def calculateUtxoFee(
      utxo: CoinSelectorUtxo,
      feeRate: FeeUnit): CurrencyUnit = {
    val approxUtxoSize = CoinSelector.approximateUtxoSize(utxo)
    feeRate * approxUtxoSize
  }

  def calcEffectiveValue(
      utxo: CoinSelectorUtxo,
      feeRate: FeeUnit): CurrencyUnit = {
    val utxoFee = calculateUtxoFee(utxo, feeRate)
    utxo.prevOut.value - utxoFee
  }
}

object CoinSelector extends CoinSelector {

  /** Cribbed from [[https://github.com/bitcoinjs/coinselect/blob/master/utils.js]] */
  def approximateUtxoSize(utxo: CoinSelectorUtxo): Long = {
    val inputBase: Long = 32 + 4 + 1 + 4
    val scriptSize: Long = utxo.redeemScriptOpt match {
      case Some(script) => script.bytes.length
      case None =>
        utxo.scriptWitnessOpt match {
          case Some(script) => script.bytes.length
          case None =>
            utxo.prevOut.scriptPubKey match {
              case _: NonWitnessScriptPubKey        => 107 // P2PKH
              case _: WitnessScriptPubKeyV0         => 107 // P2WPKH
              case _: TaprootScriptPubKey           => 64 // Single Schnorr signature
              case _: UnassignedWitnessScriptPubKey => 0 // unknown
            }
        }
    }

    inputBase + scriptSize
  }

  def selectByAlgo(
      coinSelectionAlgo: CoinSelectionAlgo,
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      longTermFeeRateOpt: Option[FeeUnit] = None): Vector[CoinSelectorUtxo] =
    coinSelectionAlgo match {
      case RandomSelection =>
        randomSelection(walletUtxos, outputs, feeRate)
      case AccumulateLargest =>
        accumulateLargest(walletUtxos, outputs, feeRate)
      case AccumulateSmallestViable =>
        accumulateSmallestViable(walletUtxos, outputs, feeRate)
      case StandardAccumulate =>
        accumulate(walletUtxos, outputs, feeRate)
      case LeastWaste =>
        longTermFeeRateOpt match {
          case Some(longTermFeeRate) =>
            selectByLeastWaste(walletUtxos, outputs, feeRate, longTermFeeRate)
          case None =>
            throw new IllegalArgumentException(
              "longTermFeeRateOpt must be defined for LeastWaste")
        }
      case SelectedUtxos(outPoints) =>
        val result = walletUtxos.foldLeft(Vector.empty[CoinSelectorUtxo]) {
          (acc, utxo) =>
            val outPoint = (utxo.outPoint.txId, utxo.outPoint.vout.toInt)
            if (outPoints(outPoint)) acc :+ utxo else acc
        }
        if (result.toSet.size < outPoints.size) {
          outPoints.foreach { outPoint =>
            if (
              !result.exists(utxo =>
                utxo.outPoint.txId == outPoint._1 && utxo.outPoint.vout.toInt == outPoint._2)
            )
              throw new IllegalArgumentException(
                s"Unknown UTXO: ${outPoint._1}:${outPoint._2}")
          }
        } else if (result.size > outPoints.size) {
          throw new IllegalArgumentException(s"Found too many UTXOs")
        }
        result
    }

  private case class CoinSelectionResults(
      waste: CurrencyUnit,
      totalSpent: CurrencyUnit,
      selection: Vector[CoinSelectorUtxo])

  implicit
  private val coinSelectionResultsOrder: Ordering[CoinSelectionResults] = {
    case (a: CoinSelectionResults, b: CoinSelectionResults) =>
      if (a.waste == b.waste) {
        a.selection.size.compare(b.selection.size)
      } else a.waste.compare(b.waste)
  }

  def selectByLeastWaste(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      longTermFeeRate: FeeUnit
  ): Vector[CoinSelectorUtxo] = {
    val target = outputs.map(_.value).sum
    val results = CoinSelectionAlgo.independentAlgos.flatMap { algo =>
      // Skip failed selection attempts
      Try {
        val selection =
          selectByAlgo(algo,
                       walletUtxos,
                       outputs,
                       feeRate,
                       Some(longTermFeeRate))

        // todo for now just use 1 sat, when we have more complex selection algos
        // that just don't result in change (Branch and Bound), this will need to be calculated
        val changeCostOpt = Some(Satoshis.one)

        val waste = calculateSelectionWaste(selection,
                                            changeCostOpt,
                                            target,
                                            feeRate,
                                            longTermFeeRate)

        val totalSpent = selection.map(_.prevOut.value).sum
        CoinSelectionResults(waste, totalSpent, selection)
      }.toOption
    }

    require(
      results.nonEmpty,
      s"Not enough value in given outputs to make transaction spending $target plus fees")

    results.min.selection
  }

  /** Compute the waste for this result given the cost of change
    * and the opportunity cost of spending these inputs now vs in the future.
    * If change exists, waste = changeCost + inputs * (effective_feerate - long_term_feerate)
    * If no change, waste = excess + inputs * (effective_feerate - long_term_feerate)
    * where excess = totalEffectiveValue - target
    * change_cost = effective_feerate * change_output_size + long_term_feerate * change_spend_size
    *
    * Copied from
    * @see https://github.com/achow101/bitcoin/blob/4f5ad43b1e05cd7b403f87aae4c4d42e5aea810b/src/wallet/coinselection.cpp#L345
    *
    * @param utxos The selected inputs
    * @param changeCostOpt The cost of creating change and spending it in the future. None if there is no change.
    * @param target The amount targeted by the coin selection algorithm.
    * @param longTermFeeRate The expected average fee rate used over the long term
    * @return The waste
    */
  def calculateSelectionWaste(
      utxos: Vector[CoinSelectorUtxo],
      changeCostOpt: Option[CurrencyUnit],
      target: CurrencyUnit,
      feeRate: FeeUnit,
      longTermFeeRate: FeeUnit): CurrencyUnit = {
    require(
      utxos.nonEmpty,
      "This function should not be called with empty inputs as that would mean the selection failed")

    val (waste, selectedEffectiveValue) =
      utxos.foldLeft((CurrencyUnits.zero, CurrencyUnits.zero)) {
        case ((waste, selectedEffectiveValue), utxo) =>
          val fee = calculateUtxoFee(utxo, feeRate)
          val longTermFee = calculateUtxoFee(utxo, longTermFeeRate)
          val effectiveValue = calcEffectiveValue(utxo, feeRate)

          val newWaste = waste + fee - longTermFee
          val newSelectedEffectiveValue =
            selectedEffectiveValue + effectiveValue

          (newWaste, newSelectedEffectiveValue)
      }

    changeCostOpt match {
      case Some(changeCost) =>
        // Consider the cost of making change and spending it in the future
        // If we aren't making change, the caller should've set changeCost to 0
        require(changeCost > Satoshis.zero,
                "Cannot have a change cost less than 1")
        waste + changeCost
      case None =>
        // When we are not making change (changeCost == 0), consider the excess we are throwing away to fees
        require(selectedEffectiveValue >= target)
        waste + selectedEffectiveValue - target
    }
  }
}
