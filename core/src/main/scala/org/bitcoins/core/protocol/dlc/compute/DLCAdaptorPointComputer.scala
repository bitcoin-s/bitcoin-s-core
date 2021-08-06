package org.bitcoins.core.protocol.dlc.compute

import org.bitcoins.core.protocol.dlc.models.{
  EnumContractDescriptor,
  NumericContractDescriptor,
  SingleContractInfo
}
import org.bitcoins.core.protocol.tlv.{
  EnumOutcome,
  OracleEventV0TLV,
  SignedNumericOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Responsible for optimized computation of DLC adaptor point batches. */
object DLCAdaptorPointComputer {

  private val base: Int = 2

  private lazy val numericPossibleOutcomes: Vector[ByteVector] = {
    0
      .until(base)
      .toVector
      .map(_.toString)
      .map(CryptoUtil.serializeForHash)
  }

  /** Computes:
    *     nonce + outcomeHash*pubKey
    * where outcomeHash is as specified in the DLC spec.
    * @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Oracle.md#signing-algorithm
    */
  def computePoint(
      pubKey: SchnorrPublicKey,
      nonce: ECPublicKey,
      outcome: ByteVector): ECPublicKey = {
    val hash = CryptoUtil
      .sha256SchnorrChallenge(
        nonce.schnorrNonce.bytes ++ pubKey.bytes ++ CryptoUtil
          .sha256DLCAttestation(outcome)
          .bytes)
      .bytes

    nonce.add(pubKey.publicKey.multiply(FieldElement(hash)))
  }

  /** This trie is used for computing adaptor points for a single oracle corresponding
    * to digit prefixes while memoizing partial sums.
    *
    * For example the point corresponding to 0110 and 01111010 both begin with
    * the 011 sub-sum.
    *
    * This trie stores all already computed sub-sums and new points are computed
    * by extending this Trie.
    *
    * Note that this method should not be used if you have access to LibSecp256k1CryptoRuntime
    * because calling CryptoUtil.combinePubKeys will outperform memoization in that case.
    */
  case class AdditionTrieNode(
      preComputeTable: Vector[Vector[ECPublicKey]], // Nonce -> Outcome -> Point
      depth: Int = 0,
      private var children: Vector[AdditionTrieNode] = Vector.empty,
      private var pointOpt: Option[SecpPoint] = None) {

    /** Populates children field with base empty nodes.
      *
      * To avoid unnecessary computation (and recursion),
      * this should be called lazily only when children are needed.
      */
    def initChildren(): Unit = {
      children = 0
        .until(base)
        .toVector
        .map(_ => AdditionTrieNode(preComputeTable, depth + 1))
    }

    /** Uses the preComputeTable to calculate the adaptor point
      * for the given digit prefix.
      *
      * This is done by traversing (and where need be extending) the
      * Trie according to the digits until the point corresponding to
      * the input digits is reached.
      */
    def computeSum(digits: Vector[Int]): ECPublicKey = {
      val point = pointOpt.get

      if (digits.isEmpty) { // Then we have arrived at our result
        point match {
          case SecpPointInfinity =>
            throw new IllegalArgumentException(
              "Sum cannot be point at infinity.")
          case point: SecpPointFinite => point.toPublicKey
        }
      } else {
        val digit = digits.head
        if (children.isEmpty) initChildren()
        val child = children(digit)
        // If child is not defined, extend the trie
        child.pointOpt match {
          case Some(_) => ()
          case None =>
            val pointToAdd = preComputeTable(depth)(digit).toPoint
            child.pointOpt = Some(point.add(pointToAdd))
        }
        // Then move down and continue computation
        child.computeSum(digits.tail)
      }
    }
  }

  object AdditionTrieNode {

    /** Creates a fresh AdditionTreeNode for a given preComputeTable */
    def makeRoot(
        preComputeTable: Vector[Vector[ECPublicKey]]): AdditionTrieNode = {
      AdditionTrieNode(preComputeTable, pointOpt = Some(SecpPointInfinity))
    }
  }

  /** Efficiently computes all adaptor points, in order, for a given ContractInfo.
    * @see https://medium.com/crypto-garage/optimizing-numeric-outcome-dlc-creation-6d6091ac0e47
    */
  def computeAdaptorPoints(
      contractInfo: SingleContractInfo): Vector[ECPublicKey] = {
    // The possible messages a single nonce may be used to sign
    val possibleOutcomes: Vector[ByteVector] =
      contractInfo.contractDescriptor match {
        case enumEvent: EnumContractDescriptor =>
          enumEvent.keys.map(_.outcome).map(CryptoUtil.serializeForHash)
        case _: NumericContractDescriptor => numericPossibleOutcomes
      }

    // Oracle -> Nonce -> Outcome -> SubSigPoint
    // These are the points that are then combined to construct aggregate points.
    val preComputeTable: Vector[Vector[Vector[ECPublicKey]]] =
      contractInfo.oracleInfo.singleOracleInfos.map { info =>
        val announcement = info.announcement
        val pubKey = announcement.publicKey
        val nonces = announcement.eventTLV match {
          case v0: OracleEventV0TLV =>
            v0.nonces.map(_.publicKey)
        }

        nonces.map { nonce =>
          possibleOutcomes.map { outcome =>
            computePoint(pubKey, nonce, outcome)
          }
        }
      }

    lazy val additionTries = preComputeTable.map { table =>
      AdditionTrieNode.makeRoot(table)
    }

    val oraclesAndOutcomes = contractInfo.allOutcomes.map(_.oraclesAndOutcomes)

    oraclesAndOutcomes.map { oracleAndOutcome =>
      // For the given oracleAndOutcome, look up the point in the preComputeTable
      val subSigPoints = CryptoUtil.cryptoContext match {
        case CryptoContext.LibSecp256k1 =>
          oracleAndOutcome.flatMap { case (info, outcome) =>
            val oracleIndex =
              contractInfo.oracleInfo.singleOracleInfos.indexOf(info)
            val outcomeIndices = outcome match {
              case outcome: EnumOutcome =>
                Vector(
                  contractInfo.contractDescriptor
                    .asInstanceOf[EnumContractDescriptor]
                    .keys
                    .indexOf(outcome)
                )
              case UnsignedNumericOutcome(digits) => digits
              case _: SignedNumericOutcome =>
                throw new UnsupportedOperationException(
                  "Signed numeric outcomes not supported!")
            }

            outcomeIndices.zipWithIndex.map { case (outcomeIndex, nonceIndex) =>
              preComputeTable(oracleIndex)(nonceIndex)(outcomeIndex)
            }
          }
        case CryptoContext.BouncyCastle | CryptoContext.BCrypto =>
          oracleAndOutcome.map { case (info, outcome) =>
            val oracleIndex =
              contractInfo.oracleInfo.singleOracleInfos.indexOf(info)

            outcome match {
              case outcome: EnumOutcome =>
                val outcomeIndex = contractInfo.contractDescriptor
                  .asInstanceOf[EnumContractDescriptor]
                  .keys
                  .indexOf(outcome)

                preComputeTable(oracleIndex)(0)(outcomeIndex)
              case UnsignedNumericOutcome(digits) =>
                additionTries(oracleIndex).computeSum(digits)
              case _: SignedNumericOutcome =>
                throw new UnsupportedOperationException(
                  "Signed numeric outcomes not supported!")
            }
          }
      }

      CryptoUtil.combinePubKeys(subSigPoints)
    }
  }
}
