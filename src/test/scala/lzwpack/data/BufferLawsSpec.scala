package lzwpack.data

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

class BufferLawTests extends CatsSuite {
  // A generator for arbitrary buffers
  implicit def arbBuffer: Arbitrary[Buffer] = Arbitrary {
    for {
      bs <- Gen.posNum[Int]
    } yield Buffer(bs)
  }

  implicit def eqBuffer: Eq[Buffer] = Eq.fromUniversalEquals

  // Checks that our monoid for Buffer fulfills the monoid laws
  checkAll("Buffer.MonoidLaws", MonoidTests[Buffer].monoid)
}

