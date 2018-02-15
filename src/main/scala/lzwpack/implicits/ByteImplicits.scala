package lzwpack.implicits

import java.lang.{Byte => JavaByte}

/**
  * Provides extension functions for the standard [[scala.Byte]] type.
  */
trait ByteImplicits {
  implicit class ByteOps(b: Byte) {
    /**
      * Converts a signed JVM byte into its unsigned representation.
      */
    def unsigned: Int = JavaByte.toUnsignedInt(b)
  }
}
