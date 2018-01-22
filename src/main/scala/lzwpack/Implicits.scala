package lzwpack

object Implicits {
  implicit class ByteOps(b: Byte) {
    def unsigned = java.lang.Byte.toUnsignedInt(b)
  }
}
