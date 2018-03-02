package lzwpack

import cats.{Applicative, Foldable, ~>}
import cats.implicits._
import cats.kernel.Eq
import lzwpack.data._

/**
  * Companion object for {@see Alphabet}.
  */
object Alphabet {
  /**
    * Returns a new alphabet from some foldable sequence.
    */
  def apply[A](as: Seq[A]): Alphabet[A] = ListVector.seq(as)

  implicit val Alphanumeric: Alphabet[Char] = Alphabet(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z'))
  implicit val AllChars: Alphabet[Char] = Alphabet(Char.MinValue to Char.MaxValue)
  // Instead of bytes (8-bit signed integers), we represent byte values by their unsigned int (32-bit) representation.
  implicit val AllBytes: Alphabet[Byte] = Alphabet(1 to 255).map(_.toByte)

  // This alphabet is compatible with compress(1) tool as we reserve a code outside the dictionary
  implicit val Compress: Alphabet[Byte] = AllBytes + (0: Byte)
}

trait AlphabetInstances {
  def alphabetEq[A: Eq]: Eq[Alphabet[A]] = Eq.by[Alphabet[A], ListVector[A]](identity)
}

trait AlphabetSyntax {
  /**
    * Extension methods for {@see Alphabet}.
    */
  implicit class Ops[A](a: Alphabet[A]) {
    /**
      * Lifts this alphabet into an alphabet of applicative functors.
      */
    def pure[F[_]](implicit F: Applicative[F]): Alphabet[F[A]] = a.map(F.pure)
  }
}

