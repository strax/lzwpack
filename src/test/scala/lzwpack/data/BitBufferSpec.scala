package lzwpack.data

import lzwpack._

class BitBufferSpec extends UnitSpec {
  describe("constructor") {
    it("creates a 8-bit (unsigned) buffer from a byte") {
      val buf = BitBuffer(-0x1: Byte)
      assert(buf.data == 0xff)
      assert(buf.size == 8)
    }

    it("creates a variable-length buffer from an int") {
      val buf = BitBuffer(0xfff: Int)
      assert(buf.data == 0xfff)
      assert(buf.size == 12)
    }
  }

  describe("read") {
    val buf = BitBuffer(0xfef) // 0b111111101111, 12 bits

    it("returns an unsigned int that contains the read bits") {
      assert(buf.read(4)._1 == 0xf)
    }

    it("returns a new buffer with the remaining bits") {
      val remainder = buf.read(4)._2
      assert(remainder.size == 12 - 4)
      assert(remainder.data == (buf.data >> 4))
    }
  }

  describe("append") {
    val a = BitBuffer(b"10101010")
    val b = BitBuffer(b"1111")

    it("append the given bytes to the receiver") {
      val ab = a.append(b)
      assert(ab.data == b"111110101010")
      assert(ab.size == 12)
    }
  }

  describe("drain") {
    it("returns as much values with the given bit size as possible from the buffer") {
      val buf = BitBuffer(b"00000011110000111110111010")
      val (_, values) = buf.drain(4)
      assert(values sameElements Array(0xa, 0xb, 0xf, 0, 0xf))
    }

    it("returns an empty array if the given bit size is larger than the buffer's size") {
      assert(BitBuffer(b"10").drain(4)._2.isEmpty)
    }

    it("asserts that the bit size is positive") {
      val buf = BitBuffer(b"10")
      assertThrows[AssertionError](buf.drain(0))
      assertThrows[AssertionError](buf.drain(-1))
    }
  }
}

