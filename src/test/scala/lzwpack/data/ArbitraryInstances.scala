package lzwpack.data

import org.scalacheck.{Arbitrary, Cogen}
import cats.implicits._

/**
  * scalacheck [[Arbitrary]] instances for `data` package
  */
trait ArbitraryInstances {
  import Arbitrary._

  implicit def arbListVector[A: Arbitrary]: Arbitrary[ListVector[A]] = Arbitrary {
    for {
      as <- arbitrary[List[A]]
    } yield as.map(ListVector(_)).foldK
  }

  // Given a Cogen[A] and a Gen[A] Scalacheck can automatically make a Gen[A => B]
  implicit def cogenListVector[A: Cogen]: Cogen[ListVector[A]] = Cogen.it(_.iterator)
}
