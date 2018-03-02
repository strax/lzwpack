import cats.Applicative
import fs2.Pipe
import lzwpack.AlphabetSyntax
import lzwpack.data.ListVector
import lzwpack.implicits.AllImplicits

import scala.collection.immutable.List

package object lzwpack extends AllImplicits with DictInstances with AlphabetSyntax {
  val MaxCodeSize = 16 // bits

  /**
    * A {@see Code} represents the outputted code for a given input subsequence.
    */
  type Code = Int

  type Bytes = ListVector[Byte]

  object Bytes {
    def apply(b: Byte): Bytes = ListVector(b)
    def empty: Bytes = ListVector()
  }

  /**
    * An alphabet for a given input S is the set of possible members an input sequence can be composed from.
    *
    * @todo Implement Set
    */
  type Alphabet[A] = ListVector[A]

  def compressAdaptive[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Byte] =
    stream => stream.throughPure(LZW.compress).throughPure(Format.pack)

  def decompressAdaptive[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Byte] =
    stream => stream.throughPure(Format.unpack).throughPure(LZW.decompress)
}
