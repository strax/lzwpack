package lzwpack.data

import cats._
import cats.implicits._
import lzwpack.UnitSpec
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalatest.prop.Checkers
import Arbitrary._
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.FunctorTests

class ListVectorSpec extends UnitSpec with Checkers {
  describe("head") {
    it("returns the first element of the vector") {
      assert(ListVector(1,2,3).head == 1)
    }

    it("throws a NoSuchElementException if the vector is empty") {
      assertThrows[IndexOutOfBoundsException](ListVector().head)
    }
  }

  describe("tail") {
    it("returns a new vector with all the elements but the first one") {
      assert(ListVector(1,2,3).tail.head == 2)
      assert(ListVector(1,2,3).tail.tail.head == 3)
    }
  }

  implicit def arbListVector[A: Arbitrary]: Arbitrary[ListVector[A]] = Arbitrary {
    for {
      as <- arbitrary[List[A]]
    } yield as.map(ListVector(_)).foldK
  }

  it("forms an Eq") {
    check { EqTests[ListVector[String]].eqv }
  }

  it("forms a Functor") {
    check { FunctorTests[ListVector].functor[String, String, String] }
  }
}
