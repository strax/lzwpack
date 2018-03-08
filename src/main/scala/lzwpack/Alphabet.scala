package lzwpack

import cats.{Applicative}
import cats.kernel.Eq
import lzwpack.data._

/**
  * Companion object for [[Alphabet]]
  */
object Alphabet {
  import ListVector.range

  /**
    * Returns a new alphabet from some foldable sequence.
    */
  def apply[A](as: Seq[A]): Alphabet[A] = ListVector.seq(as)

  implicit val Alphanumeric: Alphabet[Char] = Alphabet(range('0', '9') ++ range('a', 'z') ++ range('A', 'Z'))
  implicit val AllChars: Alphabet[Char] = Alphabet(range(Char.MinValue, Char.MaxValue))
  // Instead of bytes (8-bit signed integers), we represent byte values by their unsigned int (32-bit) representation.
  implicit val AllBytes: Alphabet[Byte] = Alphabet(range(1, 255)).map(_.toByte)

  // This alphabet is compatible with compress(1) tool as we reserve a code outside the dictionary
  implicit val Compress: Alphabet[Byte] = AllBytes + (0: Byte)
}

trait AlphabetInstances {
  def alphabetEq[A: Eq]: Eq[Alphabet[A]] = Eq.by[Alphabet[A], ListVector[A]](identity)
}

trait AlphabetSyntax {
  /**
    * Extension methods for [[Alphabet]].
    */
  implicit class Ops[A](a: Alphabet[A]) {
    /**
      * Lifts this alphabet into an alphabet of applicative functors.
      */
    def pure[F[_]](implicit F: Applicative[F]): Alphabet[F[A]] = a.map(F.pure)
  }
}

