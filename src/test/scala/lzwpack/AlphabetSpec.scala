package lzwpack

import lzwpack.data._
import lzwpack.data.ListVector

class AlphabetSpec extends UnitSpec {
  describe("apply") {
    it("returns the alphabet for an input string") {
      assert(Alphabet("ABC") == ListVector('A', 'B', 'C'))
    }

    it("returns an alphabet from a sequence") {
      assert(Alphabet(1 to 5) == ListVector(1, 2, 3, 4, 5))
    }
  }
}
