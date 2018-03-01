package lzwpack.data

import lzwpack.UnitSpec
import org.scalacheck.Arbitrary
import Arbitrary._
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.{FoldableTests, FunctorTests, TraverseTests}
import cats.instances.all._

class ListVectorSpec extends UnitSpec with ArbitraryInstances {
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

  describe("Eq") {
    properties { EqTests[ListVector[String]].eqv }
  }

  describe("Functor") {
    properties { FunctorTests[ListVector].functor[String, String, String] }
  }

  describe("Foldable") {
    properties { FoldableTests[ListVector].foldable[Int, Int] }
  }

  describe("Traverse") {
    properties { TraverseTests[ListVector].traverse[Int, Int, Int, Int, Option, Option] }
  }
}
