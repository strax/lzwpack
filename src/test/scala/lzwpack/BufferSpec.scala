package lzwpack

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import BitPacking._
import org.scalacheck.{Arbitrary, Gen}

class BufferSpec extends UnitSpec {
  describe("buffer()") {
    it("creates a 8-bit (unsigned) buffer from a byte") {
      val buf = buffer(-0x1: Byte)
      assert(buf.data == 0xff)
      assert(buf.size == 8)
    }

    it("creates a variable-length buffer from an int") {
      val buf = buffer(0xfff: Int)
      assert(buf.data == 0xfff)
      assert(buf.size == 12)
    }
  }

  describe("Buffer#read") {
    val buf = buffer(0xfef) // 0b111111101111, 12 bits

    it("returns an unsigned int that contains the read bits") {
      assert(buf.read(4)._1 == 0xf)
    }

    it("returns a new buffer with the remaining bits") {
      val remainder = buf.read(4)._2
      assert(remainder.size == 12 - 4)
      assert(remainder.data == (buf.data >> 4))
    }
  }

  describe("Buffer#prepend") {
    val a = buffer(b"10101010")
    val b = buffer(b"1111")

    it("prepends the given bytes to the receiver") {
      val ba = a.prepend(b)
      assert(ba.data == b"111110101010")
      assert(ba.size == 12)
    }
  }
}

class BufferLawTests extends CatsSuite {
  // A generator for arbitrary buffers
  implicit def arbBuffer: Arbitrary[BitPacking.Buffer] = Arbitrary {
    for {
      bs <- Gen.posNum[Int]
    } yield buffer(bs)
  }

  implicit def eqBuffer: Eq[Buffer] = Eq.fromUniversalEquals

  // Checks that our monoid for Buffer fulfills the monoid laws
  checkAll("Buffer.MonoidLaws", MonoidTests[Buffer].monoid)
}
