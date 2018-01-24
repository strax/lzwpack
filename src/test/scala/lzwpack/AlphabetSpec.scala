package lzwpack

import org.scalatest._

class AlphabetSpec extends UnitSpec {
  import LZW.Alphabet

  describe("apply") {
    it("returns the alphabet for an input string") {
      assert(Alphabet("ABCA") == Set('A', 'B', 'C'))
    }

    it("returns an alphabet from a sequence") {
      assert(Alphabet(1 to 5) == Set(1, 2, 3, 4, 5))
    }
  }
}
