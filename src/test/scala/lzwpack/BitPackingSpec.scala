package lzwpack

import fs2._
import Implicits._

class BitPackingSpec extends UnitSpec {
  import BitPacking._

  describe("pack") {
    it("packs numbers efficiently to bytes") {
      assertResult(List(0xAD, 0xF3, 0xF5, 0x1)) {
        Stream.emits(List(0x1,0x2,0x5,0x6,0x9,0xFAF,0)).through(pack).toList.unsigned
      }
    }
  }
}
