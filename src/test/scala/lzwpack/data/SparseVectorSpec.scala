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

  describe("find") {
    val vec: SparseVector[String] = SparseVector() + (10001 -> "foo") + (42 -> "bar") + (9004 -> "baz") + (0 -> "x")
    it("finds an entry by its index") {
      assert(vec find ((k,_) => k == 10001) contains (10001 -> "foo"))
    }
    it("finds an entry by its value") {
      assert(vec find ((_,v) => v == "foo") contains (10001 -> "foo"))
    }
    it("finds an entry with a zero index") {
      assert(vec find ((k, _) => k == 0) contains (0 -> "x"))
    }
    it("returns the first entry for which f(entry); order is not defined") {
      assert(vec find ((_, _ ) => true) contains (10001 -> "foo"))
    }
    it("returns None when no element was found") {
      assert(vec.find((k, _) => k == -1) == None)
    }
  }

  describe("size") {
    it("returns 0 for an empty SparseVector") {
      assert(SparseVector().size == 0)
    }

    it("returns the number of values stored in the SparseVector") {
      assert(SparseVector(1,2,3).size == 3)
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
