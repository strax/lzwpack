package lzwpack

import lzwpack.data.HashMapVector
import org.scalatest._

class CompressionDictSpec extends UnitSpec {
  describe("empty") {
    it("returns a CompressionDict which contains no elements") {
      assert(CompressionDict.empty.size == 0)
    }

    it("returns a CompressionDict for which headIndex = 0") {
      assert(CompressionDict.empty.currentCode == 0)
    }
  }

  describe("add") {
    it("returns a new CompressionDict with incremented headIndex") {
      assert(CompressionDict[Unit](HashMapVector(), 5).add(()).currentCode == 6)
    }

    it("returns a new CompressionDict that contains the given argument") {
      assert(CompressionDict.empty[String].add("foo").contains("foo"))
    }
  }

  describe("contains") {
    it("returns true if this CompressionDict contains the given argument") {
      assert(CompressionDict(HashMapVector("a" -> 1), 1).contains("a"))
    }

    it("returns false if this CompressionDict does not contain the given argument") {
      assert(!CompressionDict[String](HashMapVector(), 0).contains("a"))
    }
  }
}
