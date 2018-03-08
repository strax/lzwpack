package lzwpack.data

import lzwpack.UnitSpec
import cats.{Eq, Hash}

class HashMapVectorSpec extends UnitSpec {
  describe("contains") {
    it("returns true if this HashMapVector contains a value with the given key") {
      assert(HashMapVector("foo" -> "bar", "barr" -> "baz").contains("barr"))
    }

    it("returns false if this HashMapVector does not contain a value with the given key") {
      assert(!HashMapVector("foo" -> "bar", "barr" -> "baz").contains("bar"))
    }
  }

  describe("apply") {
    it("returns the value for the given key if it exists") {
      assert(HashMapVector("foo" -> "bar").apply("foo") === "bar")
    }

    it("throws a NoSuchElementException otherwise") {
      assertThrows[NoSuchElementException](HashMapVector("foo" -> "bar").apply("bar"))
    }
  }

  describe("get") {
    it("returns a Some value for the given key if it exists") {
      assert(HashMapVector("foo" -> "bar").get("foo") === Some("bar"))
    }

    it("returns None otherwise") {
      assert(HashMapVector("foo" -> "bar").get("bar") === None)
    }
  }

  describe("size") {
    it("returns the number of values in the hash map") {
      assert(HashMapVector(1 -> true, 2 -> true, 3 -> true).size === 3)
    }
  }

  describe("isEmpty") {
    it("returns true for empty hash maps") {
      assert(HashMapVector[Int, Int]().isEmpty)
      assert(HashMapVector.empty[Int, Int].isEmpty)
    }

    it("returns false for hash maps with at least one value") {
      assert(!HashMapVector(1 -> true).isEmpty)
    }
  }

  describe("fold") {
    it("reduces all values with a given function and initial value") {
      assert(HashMapVector(1 -> 2, 2 -> 3, 3 -> 4).fold(100) { case ((_, v), acc) => acc + v } === 100 + 4 + 3+ 2)
    }
  }

  describe("find") {
    it("returns the key and value the given predicate returns true for") {
      assert(HashMapVector(1 -> 2, 2 -> 3, 3 -> 4).find((_, v) => v === 3) === Some((2, 3)))
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
      sealed trait Mock
      val a = new Mock {
        override def hashCode(): Int = 2
      }
      val b = new Mock {
        override def hashCode(): Int = 2
      }
      implicit val mockHash: Hash[Mock] = Hash.fromUniversalHashCode
      implicit val mockEq: Eq[Mock] = Eq.fromUniversalEquals
      val map = HashMapVector(a -> "a", b -> "b")
      assert(map.get(a) === "a".some)
      assert(map.get(b) === "b".some)
    }
  }
}
