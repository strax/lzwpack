package lzwpack.data

import java.lang.{Long => JavaLong}

object Binary {
  /**
    * Returns n rounded up to nearest 2^k - 1, k ∈ {0,1,2...}.
    */
  def bitmask(n: Long): Long = {
    var v = n
    v |= v >> 1
    v |= v >> 2
    v |= v >> 4
    v |= v >> 8
    v |= v >> 16
    v |= v >> 32
    v
  }

  /**
    * Returns the amount of bits it takes to represent `n` in binary.
    */
  def bitsize(n: Long): Int = if(n == 0) 1 else JavaLong.bitCount(bitmask(n))
}