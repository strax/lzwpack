package lzwpack.implicits

import java.lang.{Long => JavaLong}

trait LongImplicits {
  implicit class LongOps(n: Long) {
    def bitLength: Int = if(n == 0L) 0 else Math.floor(Math.log(n) / Math.log(2)).toInt + 1

    def bin: String = bin(JavaLong.SIZE)
    def bin(size: Int): String = n.toBinaryString.reverse.padTo(size, "0").reverse.mkString

    def hex: String = n formatted "0x%02X"
  }
}
