package lzwpack.data

import lzwpack.UnitSpec
import org.scalacheck.Gen

class BinarySpec extends UnitSpec {
  import Binary._

  describe("bitmask") {
    it("returns the next highest number with all bits set for the given number") {
      assert(bitmask(0) == 0)
      assert(bitmask(1) == 1)
      assert(bitmask(2) == 3)
      assert(bitmask(3) == 3)
      assert(bitmask(4) == 7)
      assert(bitmask(Long.MaxValue) == Long.MaxValue)
    }
  }

  describe("bitsize") {
    it("returns the number of bits between the LSB and MSB of the given number") {
      // Note that we define bitsize(0) == 1 since it effectively takes one bit to represent a presence of zero
      assert(bitsize(0) == 1)
      assert(bitsize(1) == 1)
      assert(bitsize(2) == 2)
      assert(bitsize(3) == 2)
      assert(bitsize(4) == 3)
      assert(bitsize(7) == 3)
      assert(bitsize(8) == 4)
      // -1 == 64 bits set in a Long
      assert(bitsize(-1L) == 64)
    }
  }
}
