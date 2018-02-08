package lzwpack.data

import lzwpack.UnitSpec

class SparseVectorSpec extends UnitSpec {
  describe("contains") {
    it("returns true if the vector contains a value at the given index") {
      assert(SparseVector(1,2,3).contains(2))
    }

    it("returns false if the vector does not contain a value at the given index") {
      assert(!SparseVector(1,2,3).contains(3))
    }
  }

  describe("get") {
    it("returns the value associated with the given index, if it exists") {
      assert(SparseVector("foo", "bar", "baz").get(1).contains("bar"))
    }

    it("returns None if the vector does not contain a value at the given index") {
      assert(SparseVector("foo", "bar", "baz").get(3).isEmpty)
    }
  }

  describe("updated") {
    it("does not mutate this vector") {
      val vec = SparseVector(1, 2, 3)
      vec.updated(0, 2)
      assert(vec.get(0).contains(1))
    }

    it("returns a new vector that contains a mapping from the given index to value") {
      assert(SparseVector().updated(0, 2).get(0).contains(2))
    }

    it("overrides an existing mapping if one exists in the vector") {
      assert(SparseVector(1).updated(0, 10).get(0).contains(10))
    }

    it("allows random access") {
      assert(SparseVector(1,2,3).updated(10, 5).get(10).contains(5))
    }
  }
}
