package org.bitcoins.dlc.payouts

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen

import scala.math.BigDecimal.RoundingMode

class OutcomeValueFunctionTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "OutcomeValueFunction"

  private val numGen = Gen.choose[Double](0, 1000).map(BigDecimal(_))
  private val intGen = Gen.choose[Int](0, 1000)

  def nPoints(n: Int): Gen[Vector[OutcomeValuePoint]] = {
    val valueGen = Gen.choose[Long](0, 10000)
    val pointGen = for {
      outcome <- numGen
      value <- valueGen
    } yield OutcomeValuePoint(outcome, Satoshis(value), isEndpoint = true)
    Gen
      .listOfN(n, pointGen)
      .suchThat(points =>
        points.map(_.outcome).distinct.length == points.length)
      .map(_.toVector.sortBy(_.outcome))
  }

  it should "agree on lines and degree 1 polynomials" in {
    forAll(nPoints(2), Gen.listOfN(1000, numGen)) {
      case (Vector(point1, point2), outcomes) =>
        val line = OutcomeValueLine(point1, point2)
        val polyDegOne = OutcomeValuePolynomial(Vector(point1, point2))

        outcomes.foreach { outcome =>
          assert(line(outcome) == polyDegOne(outcome))
        }
    }
  }

  it should "agree on lines and y = mx + b" in {
    val twoNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
    } yield {
      if (num1 < num2) {
        (num1, num2)
      } else {
        (num2, num1)
      }
    }

    forAll(intGen, intGen, Gen.listOfN(1000, numGen), twoNums) {
      case (slope, yIntercept, outcomes, (x1, x2)) =>
        def func(outcome: BigDecimal): Satoshis = {
          val value = slope * outcome + yIntercept
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomeValuePoint(x1, func(x1), isEndpoint = true)
        val point2 = OutcomeValuePoint(x2, func(x2), isEndpoint = true)
        val line = OutcomeValueLine(point1, point2)

        outcomes.foreach { outcome =>
          assert(line(outcome) == func(outcome))
        }
    }
  }

  it should "agree on quadratics and degree 2 polynomials" in {
    forAll(nPoints(3), Gen.listOfN(1000, numGen)) {
      case (Vector(point1, point2, point3), outcomes) =>
        val midPoint2 = point2.copy(isEndpoint = false)
        val parabola = OutcomeValueQuadratic(point1, midPoint2, point3)
        val polyDegTwo =
          OutcomeValuePolynomial(Vector(point1, midPoint2, point3))

        outcomes.foreach { outcome =>
          assert(parabola(outcome) == polyDegTwo(outcome))
        }
    }
  }

  it should "agree on quadratics and y = ax^2 + bx + c" in {
    val threeNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
      num3 <- intGen.suchThat(x => x != num1 && x != num2)
    } yield {
      val nums = Vector(num1, num2, num3).sorted
      (nums(0), nums(1), nums(2))
    }

    forAll(intGen, intGen, intGen, Gen.listOfN(1000, numGen), threeNums) {
      case (a, b, c, outcomes, (x1, x2, x3)) =>
        def func(outcome: BigDecimal): Satoshis = {
          val value = a * outcome * outcome + b * outcome + c
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomeValuePoint(x1, func(x1), isEndpoint = true)
        val point2 = OutcomeValuePoint(x2, func(x2), isEndpoint = false)
        val point3 = OutcomeValuePoint(x3, func(x3), isEndpoint = true)
        val parabola = OutcomeValueQuadratic(point1, point2, point3)

        outcomes.foreach { outcome =>
          assert(parabola(outcome) == func(outcome))
        }
    }
  }

  it should "agree on cubics and y = ax^3 + bx^2 + cx + d" in {
    val fourNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
      num3 <- intGen.suchThat(x => x != num1 && x != num2)
      num4 <- intGen.suchThat(x => x != num1 && x != num2 && x != num3)
    } yield {
      val nums = Vector(num1, num2, num3, num4).sorted
      (nums(0), nums(1), nums(2), nums(3))
    }

    forAll(intGen,
           intGen,
           intGen,
           intGen,
           Gen.listOfN(1000, numGen),
           fourNums) {
      case (a, b, c, d, outcomes, (x1, x2, x3, x4)) =>
        def func(outcome: BigDecimal): Satoshis = {
          val value =
            a * outcome * outcome * outcome + b * outcome * outcome + c * outcome + d
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomeValuePoint(x1, func(x1), isEndpoint = true)
        val point2 = OutcomeValuePoint(x2, func(x2), isEndpoint = false)
        val point3 = OutcomeValuePoint(x3, func(x3), isEndpoint = false)
        val point4 = OutcomeValuePoint(x4, func(x4), isEndpoint = true)
        val cubic =
          OutcomeValuePolynomial(Vector(point1, point2, point3, point4))

        outcomes.foreach { outcome =>
          assert(cubic(outcome) == func(outcome))
        }
    }
  }
}
