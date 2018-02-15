package lzwpack.implicits

trait StringImplicits {
  implicit class StringOps(sc: StringContext) {
    /**
      * Convenience method for the binary literal syntax:
      * `b"0110" == 4`.
      */
    def b(_args: Any*): Int = Integer.parseUnsignedInt(sc.parts.mkString.replaceAll("""\s+""", ""), 2)
  }
}
