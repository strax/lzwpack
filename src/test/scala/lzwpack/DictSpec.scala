package lzwpack

import org.scalatest._

class DictSpec extends UnitSpec {
  import LZW.Dict

  describe("empty") {
    it("returns a Dict which contains no elements") {
      assert(Dict.empty.entries.size == 0)
    }

    it("returns a Dict for which headIndex = 0") {
      assert(Dict.empty.headIndex == 0)
    }
  }

  describe("add") {
    it("returns a new Dict with incremented headIndex") {
      assert(Dict[Unit](Map(), 5).add(()).headIndex == 6)
    }

    it("returns a new Dict that contains the given argument") {
      assert(Dict.empty[String].add("foo").contains("foo"))
    }
  }

  describe("contains") {
    it("returns true if this Dict contains the given argument") {
      assert(Dict(Map("a" -> 1), 1).contains("a"))
    }

    it("returns false if this Dict does not contain the given argument") {
      assert(!Dict[String](Map(), 0).contains("a"))
    }
  }
}
