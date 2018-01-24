package lzwpack

import org.scalatest._

class AlphabetSpec extends UnitSpec {
  describe("apply") {
    it("returns the alphabet for an input string") {
      assert(Alphabet("ABC") == List('A', 'B', 'C'))
    }

    it("returns an alphabet from a sequence") {
      assert(Alphabet(1 to 5) == List(1, 2, 3, 4, 5))
    }
  }
}
