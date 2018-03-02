package lzwpack.data

import lzwpack.UnitSpec
import cats.syntax.option._
import cats.instances.all._
import cats.Eq

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
      sealed trait Mock
      val a = new Mock {
        override def hashCode(): Int = 2
      }
      val b = new Mock {
        override def hashCode(): Int = 2
      }
      implicit val mockEq: Eq[Mock] = Eq.fromUniversalEquals
      val map = HashMapVector(a -> "a", b -> "b")
      assert(map.get(a) === "a".some)
      assert(map.get(b) === "b".some)
    }
  }
}
