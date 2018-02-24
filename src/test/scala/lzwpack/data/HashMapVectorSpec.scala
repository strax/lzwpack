package lzwpack.data

import lzwpack.UnitSpec

class HashMapVectorSpec extends UnitSpec {
  describe("contains") {
    it("returns true if this HashMapVector contains a value with the given key") {
      assert(HashMapVector("foo" -> "bar", "barr" -> "baz").contains("barr"))
    }

    it("returns false if this HashMapVector does not contain a value with the given key") {
      assert(!HashMapVector("foo" -> "bar", "barr" -> "baz").contains("bar"))
    }
  }

  describe("updated") {
    it("returns a new HashMapVector with the given element") {
      assert(HashMapVector("foo" -> "bar").updated("barr" -> "baz").get("barr").contains("baz"))
    }

    it("returns a new HashMapVector with the given key updated to the given value") {
      assert(HashMapVector("foo" -> "bar").updated("foo" -> "baz").get("foo").contains("baz"))
    }

    it("puts two elements with the same hashCode() but for which a != b to an overflow list") {
      val a = new {
        override def hashCode(): Int = 2
      }
      val b = new {
        override def hashCode(): Int = 2
      }
      val map = HashMapVector(a -> "a", b -> "b")
      assert(map.get(a) === Some("a"))
      assert(map.get(b) === Some("b"))
    }
  }
}
