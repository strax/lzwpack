package lzwpack.data

import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.implicits._

/**
  * scalacheck [[Arbitrary]] instances for `data` package
  */
trait ArbitraryInstances {
  import Arbitrary._

  implicit def arbListVector[A: Arbitrary]: Arbitrary[ListVector[A]] = Arbitrary {
    for {
      n <- Gen.chooseNum(0, 5)
      as <- Gen.listOfN(n, arbitrary[A])
    } yield as foldMap (ListVector(_))
  }

  // Given a Cogen[A] and a Gen[A] Scalacheck can automatically make a Gen[A => B]
  implicit def cogenListVector[A: Cogen]: Cogen[ListVector[A]] = Cogen.it(_.iterator)
}
