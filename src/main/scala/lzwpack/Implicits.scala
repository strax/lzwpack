package lzwpack

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
}
