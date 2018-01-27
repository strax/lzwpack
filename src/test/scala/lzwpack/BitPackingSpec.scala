package lzwpack

import fs2._

class BitPackingSpec extends UnitSpec {
  import BitPacking._

  describe("pack") {
    it("packs numbers efficiently to bytes") {
      val sequence = List((b"100001", 10), (b"111111111111111", 16), (b"1001", 6), (b"0", 0))

      assertResult(List(0x21, 0xfc, 0xff, 0x25)) {
        Stream.emits(sequence).through(pack).toList.unsigned
      }
    }
  }
}
