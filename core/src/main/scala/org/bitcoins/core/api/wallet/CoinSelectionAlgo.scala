package org.bitcoins.core.api.wallet

import org.bitcoins.crypto.{DoubleSha256Digest, StringFactory}

/** Represents the various ways the wallet can do coin selection */
sealed abstract class CoinSelectionAlgo

object CoinSelectionAlgo extends StringFactory[CoinSelectionAlgo] {

  /** Randomly selects utxos until it has enough to fund the desired amount,
    * should only be used for research purposes
    */
  case object RandomSelection extends CoinSelectionAlgo

  /** Greedily selects from walletUtxos starting with the largest outputs, skipping outputs with values
    * below their fees. Better for high fee environments than accumulateSmallestViable.
    */
  case object AccumulateLargest extends CoinSelectionAlgo

  /** Greedily selects from walletUtxos starting with the smallest outputs, skipping outputs with values
    * below their fees. Good for low fee environments to consolidate UTXOs.
    *
    * Has the potential privacy breach of connecting a ton of UTXOs to one address.
    */
  case object AccumulateSmallestViable extends CoinSelectionAlgo

  /** Greedily selects from walletUtxos in order, skipping outputs with values below their fees */
  case object StandardAccumulate extends CoinSelectionAlgo

  /** Tries all coin selection algos and uses the one with the least waste */
  case object LeastWaste extends CoinSelectionAlgo

  case class SelectedUtxos(outPoints: Set[(DoubleSha256Digest, Int)])
      extends CoinSelectionAlgo

  /** Coin selection algos that don't call other ones */
  val independentAlgos: Vector[CoinSelectionAlgo] =
    Vector(RandomSelection,
           AccumulateLargest,
           AccumulateSmallestViable,
           StandardAccumulate)

  /** Coin selection algos that call upon other ones and choose the best */
  val multiCoinSelectionAlgos: Vector[CoinSelectionAlgo] = Vector(LeastWaste)

  val all: Vector[CoinSelectionAlgo] =
    independentAlgos ++ multiCoinSelectionAlgos

  override def fromStringOpt(str: String): Option[CoinSelectionAlgo] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }

  override def fromString(string: String): CoinSelectionAlgo = {
    val algoOpt = fromStringOpt(string)
    algoOpt.getOrElse(
      sys.error(s"Could not find coin selection algorithm=${string}"))
  }
}
