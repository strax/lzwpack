package lzwpack

import cats.effect.IO
import fs2._
import lzwpack.data.BitBuffer
import org.scalactic.Prettifier

class LZWSpec extends UnitSpec {
  import LZW._

  implicit val alphabet: Alphabet[Byte] = Alphabet('A' to 'Z').map(_.toByte)

  // Input and output values taken from https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch#Encoding_2
  val uncompressed = "TOBEORNOTTOBEORTOBEORNOT".getBytes()
  val compressed = List(
    0x14 -> 5,
    0x0F -> 5,
    0x02 -> 5,
    0x05 -> 5,
    0x0F -> 5,
    0x12 -> 6,
    0x0E -> 6,
    0x0F -> 6,
    0x14 -> 6,
    0x1B -> 6,
    0x1D -> 6,
    0x1F -> 6,
    0x24 -> 6,
    0x1E -> 6,
    0x20 -> 6,
    0x22 -> 6
  )

  describe("compress") {
    it("encodes an input string correctly") {
      val stream = Stream.emits(uncompressed)
      stream.through(compress).toList should contain theSameElementsInOrderAs compressed.map(BitBuffer.tupled)
    }
  }

  describe("decompress") {
    it("decodes a compressed string correctly") {
      val stream = Stream.emits(compressed.map(_._1))
      stream.through(decompress).toList should contain theSameElementsInOrderAs uncompressed
    }
  }
}
