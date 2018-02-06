package lzwpack

import cats.effect.IO
import fs2._
import lzwpack.data.BitBuffer

class FormatSpec extends UnitSpec {
  import Format._

  describe("pack") {
    it("packs numbers efficiently to bytes") {
      val sequence = List(
        BitBuffer(b"100001", 10),
        BitBuffer(b"111111111111111", 16),
        BitBuffer(b"1001", 6),
        BitBuffer(b"0", 0)
      )

      assertResult(List(0x21, 0xfc, 0xff, 0x25)) {
        Stream.emits(sequence).through(pack).toList.unsigned
      }
    }
  }

  describe("unpack") {
    it("unpacks numbers from a byte stream") {
      // Let's use a 3-character alphabet. Then our own codes will start at 0x05 == 0b101, so we should
      // read 3-bit values at first
      implicit val alphabet: Alphabet[Byte] = Alphabet(List(1,2,3))
      val input: List[Byte] = List(
        b"11110101", // First byte: 0b101, 0b110 and 0b10 (part of 0b111)
        b"01010001", // Second byte: 0b1 (continuation), 0b1000 (switch to 4 bits), 0b010 (part of 0b0100)
        b"00001100"  // Third byte: 0b0 (continuation), 0b110
      ).map(_.toByte)

      assertResult(List(5,6,7,8,2,6)) {
        Stream.emits(input).through(unpack).toList
      }
    }
  }
}
