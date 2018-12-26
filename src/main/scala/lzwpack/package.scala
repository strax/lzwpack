import cats.Applicative
import fs2.Pipe
import lzwpack.AlphabetSyntax
import lzwpack.data.ListVector
import lzwpack.implicits.AllImplicits

import scala.collection.immutable.List

package object lzwpack extends AllImplicits with DictInstances with AlphabetSyntax {
  val MaxCodeSize = 16 // bits

  /**
    * Represents the outputted code for a given input subsequence.
    */
  type Code = Int

  /**
    * An alphabet for a given input S is the set of possible members an input sequence can be composed from.
    */
  type Alphabet[A] = ListVector[A]

  /**
    * Returns a [[Pipe]] that performs a compression and adaptive packing in one pass.
    */
  def compressAdaptive[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Byte] =
    stream => stream.through(LZW.compress).through(Format.pack)

  /**
    * Returns a [[Pipe]] that performs an adaptive unpacking and decompression in one pass.
    */
  def decompressAdaptive[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Byte] =
    stream => stream.through(Format.unpack).through(LZW.decompress)
}
