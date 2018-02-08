package lzwpack.implicits

import java.lang.{Long => JavaLong}

import lzwpack.data.Binary

trait LongImplicits {
  implicit class LongOps(n: Long) {
    def bitsize: Int = Binary.bitsize(n)

    def bin: String = bin(JavaLong.SIZE)
    def bin(size: Int): String = n.toBinaryString.reverse.padTo(size, "0").reverse.mkString

    def hex: String = n formatted "0x%02X"
  }
}
