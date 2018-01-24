package lzwpack

import fs2._

class LZWSpec extends UnitSpec {
  import LZW._

  describe("compress") {
    it("encodes an input string correctly") {
      implicit val alphabet = Alphabet('A' to 'Z')
      val input = Stream.emits("TOBEORNOTTOBEORTOBEORNOT")
      assertResult(input.through(compress).toList)(List(20, 15, 2, 5, 15, 18, 14, 15, 20, 27, 29, 31, 36, 30, 32, 34, 0))
    }
  }
}
