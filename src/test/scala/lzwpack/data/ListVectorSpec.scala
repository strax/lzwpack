package lzwpack.data

import lzwpack.UnitSpec
import org.scalacheck.{Arbitrary, Gen, Properties}
import Arbitrary._
import cats.Applicative
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import cats.laws.discipline.{ApplicativeTests, SemigroupalTests, TraverseTests}
import org.scalacheck.Prop._
import org.scalacheck.Prop.BooleanOperators

class ListVectorSpec extends UnitSpec with ArbitraryInstances {
  implicit val iso = SemigroupalTests.Isomorphisms.invariant[ListVector](Applicative[ListVector])

  describe("head") {
    it("returns the first element of the vector") {
      assert(ListVector(1, 2, 3).head == 1)
    }

    it("throws a NoSuchElementException if the vector is empty") {
      assertThrows[IndexOutOfBoundsException](ListVector().head)
    }
  }

  describe("tail") {
    it("returns a new vector with all the elements but the first one") {
      assert(ListVector(1, 2, 3).tail.head == 2)
      assert(ListVector(1, 2, 3).tail.tail.head == 3)
    }
  }

  describe("drop") {
    it("drops given amount of elements from the beginning of the list") {
      assert(ListVector(1,2,3,4,5).drop(2) === ListVector(1,2,3,4,5).tail.tail)
      assert(ListVector(1,2,3,4,5).drop(2).size === 3)
      assert(ListVector(1,2,3,4,5).drop(2) === ListVector(3,4,5))
    }

    it("returns an empty list if all the elements are dropped") {
      assert(ListVector(1,2,3).drop(4) === ListVector.empty[Int])
    }

    it("distributes over addition") {
      check {
        forAll(arbitrary[ListVector[Int]], Gen.posNum[Int], Gen.posNum[Int]) { (xs, n, m) =>
          (n + m >= 0) ==> (
            ((xs drop n) drop m) === (xs drop (n + m))
          )
        }
      }
    }
  }

  describe("take") {
    it("takes the given amount elements from the beginning of the list") {
      assert(ListVector(1,2,3,4,5).take(2) === ListVector(1,2))
    }
  }

  it("behaves the same for first dropping then taking and first taking then dropping") {
    check {
      forAll(arbitrary[ListVector[Int]], Gen.posNum[Int], Gen.posNum[Int]) { (xs, n, m) =>
        (n + m >= 0) ==> (
          xs.drop(n).take(m) === xs.take(m + n).drop(n)
        )
      }
    }
  }

  properties { EqTests[ListVector[String]].eqv }
  properties { MonoidTests[ListVector[String]].monoid }
  properties { TraverseTests[ListVector].traverse[Int, Int, Int, Int, Option, Option] }
  properties { ApplicativeTests[ListVector].applicative[String, String, String] }
}
