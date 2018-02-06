import cats.Applicative
import fs2.Pipe

import scala.collection.immutable.List

package object lzwpack extends Implicits {
  val MaxCodeSize = 16 // bits

  /**
    * A {@see Code} represents the outputted code for a given input subsequence.
    */
  type Code = Int

  type Bytes = List[Byte]

  object Bytes {
    def apply(b: Byte): Bytes = List(b)
    def empty: Bytes = List()
  }

  /**
    * An alphabet for a given input S is the set of possible members an input sequence can be composed from.
    *
    * @todo Implement Set
    */
  type Alphabet[A] = List[A]

  /**
    * Defines an implicit conversion to include Alphabet's methods by default.
    */
  implicit def alphabetOps[A](a: Alphabet[A]): Alphabet.Ops[A] = Alphabet.Ops(a)

  def compressAdaptive[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Byte] =
    stream => stream.through(LZW.compress).through(Format.pack)

  def decompressAdaptive[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Byte] =
    stream => stream.through(Format.unpack).through(LZW.decompress)
}
