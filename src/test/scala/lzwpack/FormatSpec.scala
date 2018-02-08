package lzwpack

import cats.effect.IO
import fs2._
import lzwpack.data.BitBuffer

class FormatSpec extends UnitSpec {
  import Format._

  describe("pack") {
    it("packs variable-width bit buffers into bytes in MSB order") {
      val sequence = List(
        BitBuffer(b"0000100001", 10),
        BitBuffer(b"01111111 11111111", 16),
        BitBuffer(b"001001", 6),
        BitBuffer(b"0", 0)
      )

      assertResult(List(b"00100001", b"111111 00", b"111111 11", b"0 001001 01")) {
        Stream.emits(sequence).through(pack).toList.unsigned
      }
    }
  }

  describe("unpack") {
    it("unpacks LZW-packed byte stream into unsigned integers") {
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
