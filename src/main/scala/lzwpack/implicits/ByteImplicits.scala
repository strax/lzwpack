package lzwpack.implicits

import java.lang.{Byte => JavaByte}

trait ByteImplicits {
  implicit class ByteOps(b: Byte) {
    /**
      * Converts a signed JVM byte into its unsigned representation.
      */
    def unsigned: Int = JavaByte.toUnsignedInt(b)
  }
}
