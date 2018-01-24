package lzwpack

object Implicits {
  implicit class ByteOps(b: Byte) {
    def unsigned = java.lang.Byte.toUnsignedInt(b)
  }

  implicit def byteSeqToString(bs: Seq[Byte]): String = bs.toString
}
