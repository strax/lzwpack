package lzwpack

import java.lang.{Byte => JavaByte}

/**
  * Contains extension methods to existing types, grouped into an object
  * so all the implicits can be imported with <code>import Implicits._</code>.
  */
object Implicits {
  implicit class ByteOps(b: Byte) {
    /**
      * Converts a signed JVM byte into its unsigned representation.
      */
    def unsigned: Int = JavaByte.toUnsignedInt(b)
  }

  implicit class IntOps(n: Int) {
    /**
      * Returns the bit count (from the most significant bit) of a given unsigned 32-bit integer.
      */
    def bitLength: Int = if(n == 0) 0 else Math.floor(Math.log(n) / Math.log(2)).toInt + 1

    def byteString: String = (n & 0xFF).toBinaryString.reverse.padTo(8, "0").reverse.mkString
  }

  implicit class ListOps(bs: List[Byte]) {
    def unsigned: List[Int] = bs map (_.unsigned)
  }

  implicit class StringOps(sc: StringContext) {
    def b(): Int = Integer.parseUnsignedInt(sc.parts.mkString, 2)
  }
}
