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
    def unsigned: Int = java.lang.Byte.toUnsignedInt(b)
  }

  implicit class ListOps(bs: List[Byte]) {
    def unsigned: List[Int] = bs map (_.unsigned)
  }
}
