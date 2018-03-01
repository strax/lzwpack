package lzwpack.data

import lzwpack.UnitSpec
import org.scalacheck.Arbitrary
import Arbitrary._
import cats.Applicative
import cats.kernel.laws.discipline.{EqTests, MonoidTests}
import cats.laws.discipline.{ApplicativeTests, FoldableTests, SemigroupalTests, TraverseTests}
import cats.instances.all._
import cats.laws.discipline.SemigroupalTests.Isomorphisms

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

  properties { EqTests[ListVector[String]].eqv }
  properties { MonoidTests[ListVector[String]].monoid }
  properties { TraverseTests[ListVector].traverse[Int, Int, Int, Int, Option, Option] }
  properties { ApplicativeTests[ListVector].applicative[String, String, String] }
}
