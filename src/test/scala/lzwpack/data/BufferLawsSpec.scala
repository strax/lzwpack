package lzwpack.data

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

class BitBufferLawTests extends CatsSuite {
  // A generator for arbitrary buffers
  implicit def arbBuffer: Arbitrary[BitBuffer] = Arbitrary {
    for {
      bs <- Gen.posNum[Int]
    } yield BitBuffer(bs)
  }

  implicit def eqBuffer: Eq[BitBuffer] = Eq.fromUniversalEquals

  // Checks that our monoid for Buffer fulfills the monoid laws
  checkAll("Buffer.MonoidLaws", MonoidTests[BitBuffer].monoid)
}

