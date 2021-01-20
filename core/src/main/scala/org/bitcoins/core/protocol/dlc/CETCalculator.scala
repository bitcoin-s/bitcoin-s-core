package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.core.util.{Indexed, NumberUtil}
import scodec.bits.BitVector

import scala.annotation.tailrec

object CETCalculator {

  /** Given a range and a payout function with which to build CETs,
    * the first step is to split the range into sub-ranges which
    * can be compressed or cannot be compressed, represented here
    * as CETRanges.
    *
    * These ranges are inclusive in both indices.
    */
  sealed trait CETRange {
    def indexFrom: Long
    def indexTo: Long
  }

  /** This range contains payouts all <= 0
    * (Note that interpolated functions are allowed
    * to be negative, but we set all negative values to 0).
    */
  case class StartZero(indexFrom: Long, indexTo: Long) extends CETRange

  /** This range contains payouts all >= totalCollateral */
  case class StartTotal(indexFrom: Long, indexTo: Long) extends CETRange

  /** This range contains payouts that all vary at every step and cannot be compressed */
  case class StartFunc(indexFrom: Long, indexTo: Long) extends CETRange

  /** This range contains some constant payout between 0 and totalCollateral (exclusive).
    * To be clear, indexFrom and indexTo are still inclusive values.
    */
  case class StartFuncConst(indexFrom: Long, indexTo: Long) extends CETRange

  object CETRange {

    /** Creates a new CETRange with a single element range */
    def apply(
        index: Long,
        value: Satoshis,
        totalCollateral: Satoshis): CETRange = {
      if (value <= Satoshis.zero) {
        StartZero(index, index)
      } else if (value >= totalCollateral) {
        StartTotal(index, index)
      } else {
        StartFunc(index, index)
      }
    }
  }

  /** Goes between from and to (inclusive) and evaluates function to split the
    * interval [to, from] into CETRanges.
    */
  def splitIntoRanges(
      from: Long,
      to: Long,
      totalCollateral: Satoshis,
      function: DLCPayoutCurve,
      rounding: RoundingIntervals): Vector[CETRange] = {
    var componentStart = from
    val Indexed(firstCurrentFunc, firstComponentIndex) =
      function.componentFor(from)
    var (currentFunc, componentIndex) = (firstCurrentFunc, firstComponentIndex)
    var prevFunc = currentFunc

    val rangeBuilder = Vector.newBuilder[CETRange]
    var currentRange: CETRange =
      CETRange(from, currentFunc(from, rounding), totalCollateral)

    var num = from

    def newRange(value: Satoshis): Unit = {
      rangeBuilder += currentRange
      currentRange = CETRange(num, value, totalCollateral)
    }

    def updateComponent(): Unit = {
      componentStart = num
      prevFunc = currentFunc
      componentIndex = componentIndex + 1
      currentFunc = function.functionComponents(componentIndex)
    }

    @tailrec
    def processConstantComponents(): Unit = {
      currentFunc match {
        case OutcomePayoutConstant(_, rightEndpoint) =>
          val componentEnd = rightEndpoint.outcome - 1
          val funcValue = rightEndpoint.roundedPayout

          if (funcValue <= Satoshis.zero) {
            currentRange match {
              case StartZero(indexFrom, _) =>
                currentRange = StartZero(indexFrom, componentEnd)
              case _: StartTotal | _: StartFunc | _: StartFuncConst =>
                rangeBuilder += currentRange
                currentRange = StartZero(componentStart, componentEnd)
            }
          } else if (funcValue >= totalCollateral) {
            currentRange match {
              case StartTotal(indexFrom, _) =>
                currentRange = StartTotal(indexFrom, componentEnd)
              case _: StartZero | _: StartFunc | _: StartFuncConst =>
                rangeBuilder += currentRange
                currentRange = StartTotal(componentStart, componentEnd)
            }
          } else if (num != from && funcValue == prevFunc(num - 1, rounding)) {
            currentRange match {
              case StartFunc(indexFrom, indexTo) =>
                rangeBuilder += StartFunc(indexFrom, indexTo - 1)
                currentRange = StartFuncConst(indexTo, componentEnd)
              case StartFuncConst(indexFrom, _) =>
                currentRange = StartFuncConst(indexFrom, componentEnd)
              case _: StartZero | _: StartTotal =>
                throw new RuntimeException("Something has gone horribly wrong.")
            }
          } else {
            rangeBuilder += currentRange
            currentRange = StartFuncConst(componentStart, componentEnd)
          }

          num = componentEnd + 1
          if (num != to) {
            updateComponent()
            processConstantComponents()
          }
        case _: DLCPayoutCurveComponent => ()
      }
    }

    processConstantComponents()

    while (num <= to) {
      if (num == currentFunc.rightEndpoint.outcome && num != to) {
        updateComponent()

        processConstantComponents()
      }

      val value = currentFunc(num, rounding)
      if (value <= Satoshis.zero) {
        currentRange match {
          case StartZero(indexFrom, _) =>
            currentRange = StartZero(indexFrom, num)
          case _: StartTotal | _: StartFunc | _: StartFuncConst =>
            newRange(value)
        }
      } else if (value >= totalCollateral) {
        currentRange match {
          case StartTotal(indexFrom, _) =>
            currentRange = StartTotal(indexFrom, num)
          case _: StartZero | _: StartFunc | _: StartFuncConst =>
            newRange(value)
        }
      } else if (
        num != from &&
        (num - 1 >= componentStart && value == currentFunc(num - 1,
                                                           rounding)) ||
        (num - 1 < componentStart && value == prevFunc(num - 1, rounding))
      ) {
        currentRange match {
          case StartFunc(indexFrom, indexTo) =>
            rangeBuilder += StartFunc(indexFrom, indexTo - 1)
            currentRange = StartFuncConst(num - 1, num)
          case StartFuncConst(indexFrom, _) =>
            currentRange = StartFuncConst(indexFrom, num)
          case _: StartZero | _: StartTotal =>
            throw new RuntimeException("Something has gone horribly wrong.")
        }
      } else {
        currentRange match {
          case StartFunc(indexFrom, _) =>
            currentRange = StartFunc(indexFrom, num)
          case _: StartZero | _: StartTotal | _: StartFuncConst =>
            newRange(value)
        }
      }

      num += 1
    }

    rangeBuilder += currentRange

    rangeBuilder.result()
  }

  /** Searches for an outcome which contains a prefix of digits */
  def searchForPrefix[Outcome](digits: Vector[Int], outcomes: Vector[Outcome])(
      outcomeToPrefix: Outcome => Vector[Int]): Option[Outcome] = {
    val indexOrOverByOne = NumberUtil.search(outcomes, digits, outcomeToPrefix)(
      NumberUtil.lexicographicalOrdering[Int])

    if (indexOrOverByOne == outcomes.length) {
      if (digits.startsWith(outcomeToPrefix(outcomes.last))) {
        Some(outcomes.last)
      } else None
    } else if (indexOrOverByOne == 0) {
      if (digits.startsWith(outcomeToPrefix(outcomes.head))) {
        Some(outcomes.head)
      } else None
    } else if (digits == outcomeToPrefix(outcomes(indexOrOverByOne))) {
      Some(outcomes(indexOrOverByOne))
    } else {
      if (digits.startsWith(outcomeToPrefix(outcomes(indexOrOverByOne - 1)))) {
        Some(outcomes(indexOrOverByOne - 1))
      } else None
    }
  }

  /** Searches for an UnsignedNumericOutcome corresponding to (prefixing) digits.
    * Assumes outcomes is ordered.
    */
  def searchForNumericOutcome(
      digits: Vector[Int],
      outcomes: Vector[DLCOutcomeType]): Option[UnsignedNumericOutcome] = {
    searchForPrefix(digits, outcomes) {
      case outcome: EnumOutcome =>
        throw new IllegalArgumentException(
          s"Expected Numeric Outcome, got $outcome")
      case UnsignedNumericOutcome(digits) => digits
    }.asInstanceOf[Option[UnsignedNumericOutcome]]
  }

  /** Computes the front groupings in the CETCompression
    * (with endpoint optimization but without total optimization).
    * This means the resulting outcomes cover [start, (prefix, digits[0], base-1, ..., base-1)].
    *
    * @param digits The unique digits of the range's start
    * @param base The base the digits are represented in
    */
  def frontGroupings(digits: Vector[Int], base: Int): Vector[Vector[Int]] = {
    val nonZeroDigits =
      digits.reverse.zipWithIndex.dropWhile(_._1 == 0) // Endpoint Optimization

    if (nonZeroDigits.isEmpty) { // All digits are 0
      Vector(Vector(0))
    } else {
      val fromFront = nonZeroDigits.init.flatMap {
        case (lastImportantDigit, unimportantDigits) =>
          val fixedDigits = digits.dropRight(unimportantDigits + 1)
          (lastImportantDigit + 1).until(base).map { lastDigit =>
            fixedDigits :+ lastDigit
          }
      }

      nonZeroDigits.map(_._1).reverse +: fromFront // Add Endpoint
    }
  }

  /** Computes the back groupings in the CETCompression
    * (with endpoint optimization but without total optimization).
    * This means the resulting outcomes cover [(prefix, digits[0], 0, ..., 0), end].
    *
    * @param digits The unique digits of the range's end
    * @param base The base the digits are represented in
    */
  def backGroupings(digits: Vector[Int], base: Int): Vector[Vector[Int]] = {
    val nonMaxDigits =
      digits.reverse.zipWithIndex.dropWhile(
        _._1 == base - 1
      ) // Endpoint Optimization

    if (nonMaxDigits.isEmpty) { // All digits are max
      Vector(Vector(base - 1))
    } else {
      // Here we compute the back groupings in reverse so as to use the same iteration as in front groupings
      val fromBack = nonMaxDigits.init.flatMap {
        case (lastImportantDigit, unimportantDigits) =>
          val fixedDigits = digits.dropRight(unimportantDigits + 1)
          0.until(lastImportantDigit)
            .reverse
            .toVector
            .map { lastDigit =>
              fixedDigits :+ lastDigit
            }
      }

      fromBack.reverse :+ nonMaxDigits.map(_._1).reverse // Add Endpoint
    }
  }

  /** Computes the middle groupings in the CETCompression (without total optimization).
    * This means the resulting outcomes cover
    * [(prefix, firstDigitStart + 1, 0, ..., 0), (prefix, firstDigitEnd-1, base-1, ..., base-1)].
    *
    * @param firstDigitStart The first unique digit of the range's start
    * @param firstDigitEnd The first unique digit of the range's end
    */
  def middleGrouping(
      firstDigitStart: Int,
      firstDigitEnd: Int): Vector[Vector[Int]] = {
    (firstDigitStart + 1).until(firstDigitEnd).toVector.map { firstDigit =>
      Vector(firstDigit)
    }
  }

  /** Splits off the shared prefix of start and end represented in the given base
    * and returns the shared prefix and the unique digits of start and of end.
    */
  def separatePrefix(
      start: Long,
      end: Long,
      base: Int,
      numDigits: Int): (Vector[Int], Vector[Int], Vector[Int]) = {
    val startDigits = NumberUtil.decompose(start, base, numDigits)
    val endDigits = NumberUtil.decompose(end, base, numDigits)

    val prefixDigits = startDigits
      .zip(endDigits)
      .takeWhile { case (startDigit, endDigit) => startDigit == endDigit }
      .map(_._1)

    (prefixDigits,
     startDigits.drop(prefixDigits.length),
     endDigits.drop(prefixDigits.length))
  }

  /** Runs the compression algorithm with all optimizations on the interval [start, end]
    * represented in the given base.
    */
  def groupByIgnoringDigits(
      start: Long,
      end: Long,
      base: Int,
      numDigits: Int): Vector[Vector[Int]] = {
    val (prefixDigits, startDigits, endDigits) =
      separatePrefix(start, end, base, numDigits)

    if (start == end) { // Special Case: Range Length 1
      Vector(prefixDigits)
    } else if (startDigits.forall(_ == 0) && endDigits.forall(_ == base - 1)) { // Total Optimization
      Vector(prefixDigits)
    } else if (prefixDigits.length == numDigits - 1) { // Special Case: Front Grouping = Back Grouping
      startDigits.last.to(endDigits.last).toVector.map { lastDigit =>
        prefixDigits :+ lastDigit
      }
    } else {
      val front = frontGroupings(startDigits, base)
      val middle = middleGrouping(startDigits.head, endDigits.head)
      val back = backGroupings(endDigits, base)

      val groupings = front ++ middle ++ back

      groupings.map { digits =>
        prefixDigits ++ digits
      }
    }
  }

  /** Computes the compressed set of outcomes and their corresponding payouts given
    * a base, the number of digits to be signed, the payout function, totalCollateral
    * and the range of outcomes to construct CETs for, [min, max].
    */
  def computeCETs(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      min: Long,
      max: Long): Vector[(Vector[Int], Satoshis)] = {
    val ranges = splitIntoRanges(min, max, totalCollateral, function, rounding)

    ranges.flatMap { range =>
      range match {
        case StartZero(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> Satoshis.zero)
        case StartTotal(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> totalCollateral)
        case StartFuncConst(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> function(indexFrom, rounding))
        case StartFunc(indexFrom, indexTo) =>
          indexFrom.to(indexTo).map { num =>
            NumberUtil.decompose(num, base, numDigits) -> function(num)
          }
      }
    }
  }

  /** Computes the compressed set of outcomes and their corresponding payouts given
    * a base, the number of digits to be signed, the payout function, and totalCollateral.
    */
  def computeCETs(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals): Vector[(Vector[Int], Satoshis)] = {
    val min = 0
    val max = Math.pow(base, numDigits).toLong - 1

    computeCETs(base, numDigits, function, totalCollateral, rounding, min, max)
  }

  /** Computes all combinations of threshold oracles, preserving order. */
  def combinations[T](oracles: Vector[T], threshold: Int): Vector[Vector[T]] = {
    if (oracles.length == threshold) {
      Vector(oracles)
    } else if (threshold == 1) {
      oracles.map(Vector(_))
    } else {
      combinations(oracles.tail, threshold - 1).map(vec =>
        oracles.head +: vec) ++ combinations(oracles.tail, threshold)
    }
  }

  /** Given binary digits corresponding to a CET, returns the CET's support bounds */
  def computeCETIntervalBinary(
      cet: Vector[Int],
      numDigits: Int): (Long, Long) = {
    val left = NumberUtil.fromDigits(cet, base = 2, numDigits)
    val right = left + (1L << (numDigits - cet.length)) - 1

    (left, right)
  }

  /** Given the left endpoint of a CET and the number of ignored digits,
    * computes the binary digits for the CET.
    */
  def numToVec(num: Long, numDigits: Int, ignoredDigits: Int): Vector[Int] = {
    BitVector
      .fromLong(num, numDigits)
      .toIndexedSeq
      .toVector
      .dropRight(ignoredDigits)
      .map {
        case false => 0
        case true  => 1
      }
  }

  /** Assumes that [start, end] is a Small Middle CET and returns the smallest
    * single CET covering that interval satisfying error bounds.
    */
  private def minCoverMidCET(
      start: Long,
      end: Long,
      minFail: Long,
      numDigits: Int): Vector[Int] = {
    val leftBound = start - minFail
    val leftBoundDigits = NumberUtil.decompose(leftBound, base = 2, numDigits)
    val rightBound = end + minFail
    val rightBoundDigits = NumberUtil.decompose(rightBound, base = 2, numDigits)

    // Shared prefix of start - minFail and end + minFail is the smallest CET.
    // [start, end] being a small middle CET guarantees the shared prefix is
    // at most of size maxError.
    leftBoundDigits
      .zip(rightBoundDigits)
      .takeWhile { case (d1, d2) => d1 == d2 }
      .map(_._1)
  }

  /** Assumes that [start, end] is a Small Left CET and returns the smallest
    * single CET covering that interval, on the same multiple of maxError,
    * satisfying error bounds.
    */
  private def minCoverLeftCET(
      end: Long,
      maxErrorExp: Int,
      minFail: Long,
      numDigits: Int): Vector[Int] = {
    val rightBound = end + minFail
    val rightBoundDigits = NumberUtil.decompose(rightBound, base = 2, numDigits)
    val (prefix, halvingDigits) =
      rightBoundDigits.splitAt(numDigits - maxErrorExp)

    prefix ++ halvingDigits.takeWhile(_ == 0)
  }

  /** Assumes that [start, end] is a Small Right CET and returns the smallest
    * single CET covering that interval, on the same multiple of maxError,
    * satisfying error bounds.
    */
  private def minCoverRightCET(
      start: Long,
      maxErrorExp: Int,
      minFail: Long,
      numDigits: Int): Vector[Int] = {
    val leftBound = start - minFail
    val leftBoundDigits = NumberUtil.decompose(leftBound, base = 2, numDigits)
    val (prefix, halvingDigits) =
      leftBoundDigits.splitAt(numDigits - maxErrorExp)

    prefix ++ halvingDigits.takeWhile(_ == 1)
  }

  /** For the case where all secondary oracles have the same single CET */
  private def singleCoveringCETCombinations[T](
      primaryCET: T,
      coverCET: T,
      numOracles: Int): Vector[T] = {
    Vector(primaryCET) ++ Vector.fill(numOracles - 1)(coverCET)
  }

  /** Generates all `2^num` possible vectors of length num containing
    * only the elements in and out, in order
    */
  private def inOrOutCombinations[T](
      in: T,
      out: T,
      num: Int): Vector[Vector[T]] = {
    0.until(num).foldLeft(Vector(Vector.empty[T])) {
      case (subCombinations, _) =>
        val firstIn = subCombinations.map(combos => in +: combos)
        val firstOut = subCombinations.map(combos => out +: combos)

        firstIn ++ firstOut
    }
  }

  /** For the case where secondary oracles can sign either
    * coverCETInner or coverCETOuter
    */
  private def doubleCoveringCETCombinations[T](
      primaryCET: T,
      coverCETInner: T,
      coverCETOuter: T,
      numOracles: Int): Vector[Vector[T]] = {
    inOrOutCombinations(coverCETInner, coverCETOuter, numOracles - 1).map(
      combos => primaryCET +: combos)
  }

  /** For the case where secondary oracles can sign either
    * coverCETInner or coverCETOuter, but not all can sign
    * coverCETInner, and coverCETInner is what the primary signs
    */
  private def doubleCoveringRestrictedCETCombinations[T](
      coverCETInner: T,
      coverCETOuter: T,
      numOracles: Int): Vector[Vector[T]] = {
    doubleCoveringCETCombinations(coverCETInner,
                                  coverCETInner,
                                  coverCETOuter,
                                  numOracles).tail
  }

  /** Given the primary oracle's CET, computes the set of CETs needed
    * for two oracles with an allowed difference (which is bounded).
    *
    * @param numDigits The number of binary digits signed by the oracles
    * @param cetDigits Digits corresponding to a CET for the primary oracle
    * @param maxErrorExp The exponent (of 2) representing the difference at which
    *                    non-support (failure) is guaranteed
    * @param minFailExp The exponent (of 2) representing the difference up to
    *                   which support is guaranteed
    */
  def computeCoveringCETsBinary(
      numDigits: Int,
      cetDigits: Vector[Int],
      maxErrorExp: Int,
      minFailExp: Int,
      maximizeCoverage: Boolean,
      numOracles: Int): Vector[Vector[Vector[Int]]] = {
    require(numOracles > 1,
            "For single oracle, just use your cetDigits parameter")

    val maxNum = (1L << numDigits) - 1
    val maxError = 1L << maxErrorExp
    val halfMaxError = maxError >> 1
    val minFail = 1L << minFailExp

    val (start, end) = computeCETIntervalBinary(cetDigits, numDigits)

    if (end - start + 1 < maxError) { // case: Small CET
      // largest multiple of maxErrorExp < start
      val leftErrorCET = (start >> maxErrorExp) << maxErrorExp
      // smallest multiple minus one of maxErrorExp > end
      val rightErrorCET = leftErrorCET + maxError - 1
      // CET of width maxError covering [leftErrorCET, rightErrorCET]
      val errorCET = numToVec(leftErrorCET, numDigits, maxErrorExp)
      /* Picture of errorCET's interval on which [start, end] resides:
       * __________________________________________________________________________
       * |                  |                                  |                  |
       * leftErrorCET   (leftErrorCET + minFail)     (rightErrorCET - minFail)  rightErrorCET
       */

      if (start >= leftErrorCET + minFail && end <= rightErrorCET - minFail) { // case: Middle CET
        val coverCET = if (maximizeCoverage) {
          errorCET
        } else {
          minCoverMidCET(start, end, minFail, numDigits)
        }

        Vector(singleCoveringCETCombinations(cetDigits, coverCET, numOracles))
      } else if (start < leftErrorCET + minFail) { // case: Left CET
        val coverCET = if (maximizeCoverage) {
          errorCET
        } else {
          minCoverLeftCET(end, maxErrorExp, minFail, numDigits)
        }

        lazy val leftCET = if (maximizeCoverage) {
          // CET of width halfMaxError covering [leftErrorCET - halfMaxError, leftErrorCET - 1]
          numToVec(leftErrorCET - halfMaxError, numDigits, maxErrorExp - 1)
        } else {
          // CET of width minFail covering at least [start - minFail, leftErrorCET - 1]
          minCoverRightCET(start, maxErrorExp, minFail, numDigits)
        }

        if (leftErrorCET == 0) { // special case: Leftmost CET
          Vector(singleCoveringCETCombinations(cetDigits, coverCET, numOracles))
        } else {
          doubleCoveringCETCombinations(cetDigits,
                                        coverCET,
                                        leftCET,
                                        numOracles)
        }
      } else if (end > rightErrorCET - minFail) { // case: Right CET
        val coverCET = if (maximizeCoverage) {
          errorCET
        } else {
          minCoverRightCET(start, maxErrorExp, minFail, numDigits)
        }

        lazy val rightCET = if (maximizeCoverage) {
          // CET of width halfMaxError covering [rightErrorCET + 1, rightErrorCET + halfMaxError]
          numToVec(rightErrorCET + 1, numDigits, maxErrorExp - 1)
        } else {
          // CET of width minFail covering at least [rightErrorCET + 1, end + minFail]
          minCoverLeftCET(end, maxErrorExp, minFail, numDigits)
        }

        if (rightErrorCET == maxNum) { // special case: Rightmost CET
          Vector(singleCoveringCETCombinations(cetDigits, coverCET, numOracles))
        } else {
          doubleCoveringCETCombinations(cetDigits,
                                        coverCET,
                                        rightCET,
                                        numOracles)
        }
      } else {
        throw new RuntimeException(
          s"Unexpected case: $cetDigits, $numDigits, $maxErrorExp, $minFailExp")
      }
    } else { // case: Large CET
      val builder = Vector.newBuilder[Vector[Vector[Int]]]

      /* zoom of left corner of [start, end]:
       * _____________________________________________________
       * |                         |                         |
       * (start - halfMaxError)   start   (start + halfMaxError)
       *
       * Secondary oracle can be on left side if primary is on right.
       */
      if (start != 0) {
        val leftInnerCET = if (maximizeCoverage) {
          numToVec(start, numDigits, maxErrorExp - 1)
        } else {
          numToVec(start, numDigits, minFailExp)
        }
        val leftCET = if (maximizeCoverage) {
          numToVec(start - halfMaxError, numDigits, maxErrorExp - 1)
        } else {
          numToVec(start - minFail, numDigits, minFailExp)
        }

        builder.++=(
          doubleCoveringRestrictedCETCombinations(leftInnerCET,
                                                  leftCET,
                                                  numOracles))
      }

      /* [start, end]:
       * ____________________________________________________
       * |                                                  |
       * start                                            end
       *
       * If both oracles are in this range (corresponding to a fixed
       * value payout) then this any difference can be ignored.
       */
      builder.+=(
        singleCoveringCETCombinations(cetDigits, cetDigits, numOracles))

      /* zoom of right corner of [start, end]:
       * _________________________________________________________
       * |                           |                           |
       * (end - halfMaxError + 1)   end   (end + halfMaxError - 1)
       *
       * Secondary oracle can be on right side if primary is on left.
       */
      if (end != maxNum) {
        val rightInnerCET = if (maximizeCoverage) {
          numToVec(end - halfMaxError + 1, numDigits, maxErrorExp - 1)
        } else {
          numToVec(end - minFail + 1, numDigits, minFailExp)
        }
        val rightCET = if (maximizeCoverage) {
          numToVec(end + 1, numDigits, maxErrorExp - 1)
        } else {
          numToVec(end + 1, numDigits, minFailExp)
        }

        builder.++=(
          doubleCoveringRestrictedCETCombinations(rightInnerCET,
                                                  rightCET,
                                                  numOracles))
      }

      builder.result()
    }
  }

  /** Given the primary oracle's CETs, computes the set of CETs needed
    * for n oracles with an allowed difference (which is bounded).
    *
    * @param numDigits The number of binary digits signed by the oracles
    * @param primaryCETs CETs corresponding to the primary oracle
    * @param maxErrorExp The exponent (of 2) representing the difference at which
    *                    non-support (failure) is guaranteed
    * @param minFailExp The exponent (of 2) representing the difference up to
    *                   which support is guaranteed
    * @param maximizeCoverage The flag that determines whether CETs used in the
    *                         non-middle small case are maximal or minimal in size
    * @param numOracles The total number of oracles (including primary) needed for execution
    */
  def computeMultiOracleCETsBinary(
      numDigits: Int,
      primaryCETs: Vector[(Vector[Int], Satoshis)],
      maxErrorExp: Int,
      minFailExp: Int,
      maximizeCoverage: Boolean,
      numOracles: Int): Vector[(Vector[Vector[Int]], Satoshis)] = {
    require(minFailExp < maxErrorExp)

    primaryCETs.flatMap {
      case (cetDigits, payout) =>
        computeCoveringCETsBinary(numDigits,
                                  cetDigits,
                                  maxErrorExp,
                                  minFailExp,
                                  maximizeCoverage,
                                  numOracles)
          .map((_, payout))
    }
  }

  /** Computes the set of CETs needed for numOracles oracles with an
    * allowed difference (which is bounded).
    *
    * @param numDigits The number of binary digits signed by the oracles
    * @param function The DLCPayoutCurve to use with primary oracles
    * @param totalCollateral The funding output's value (ignoring fees)
    * @param rounding The rounding intervals to use when computing CETs
    * @param maxErrorExp The exponent (of 2) representing the difference at which
    *                    non-support (failure) is guaranteed
    * @param minFailExp The exponent (of 2) representing the difference up to
    *                   which support is guaranteed
    * @param maximizeCoverage The flag that determines whether CETs used in the
    *                         non-middle small case are maximal or minimal in size
    * @param numOracles The total number of oracles (including primary) needed for execution
    */
  def computeMultiOracleCETsBinary(
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      maxErrorExp: Int,
      minFailExp: Int,
      maximizeCoverage: Boolean,
      numOracles: Int): Vector[(Vector[Vector[Int]], Satoshis)] = {
    val primaryCETs =
      computeCETs(base = 2, numDigits, function, totalCollateral, rounding)
    computeMultiOracleCETsBinary(numDigits,
                                 primaryCETs,
                                 maxErrorExp,
                                 minFailExp,
                                 maximizeCoverage,
                                 numOracles)
  }
}
