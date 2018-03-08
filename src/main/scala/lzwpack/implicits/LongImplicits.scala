package lzwpack.implicits

import java.lang.{Long => JavaLong}

import lzwpack.data.{Binary, ListVector}

trait LongImplicits {
  implicit class LongOps(n: Long) {
    /**
      * Returns the amount of significant bits in this Long.
      */
    def bitsize: Int = Binary.bitsize(n)

    /**
      * Converts this Long into its binary representation.
      */
    def bin: String = bin(JavaLong.SIZE)
    def bin(size: Int): String = n.toBinaryString.reverse.padTo(size, "0").reverse.mkString

    /**
      * Converts this Long into its hexadecimal representation.
      */
    def hex: String = n formatted "0x%02X"
  }
}
