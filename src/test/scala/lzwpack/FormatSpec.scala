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
      // Let's use a 5-character alphabet (excluding a reserved control character).
      // Then our own codes will start at 0x06 == 0b110, so we should read 4 3-bit values at first, then switch
      // to 4 bytes
      implicit val alphabet: Alphabet[Byte] = Alphabet(List(1,2,3,4,5))
      val input: List[Byte] = List(
        b"11 110 101",
        b"010 1000 1",
        b"000 0110 0"
      ).map(_.toByte)

      assertResult(List(5,6,7,8,2,6)) {
        Stream.emits(input).through(unpack).toList
      }
    }
  }
}
