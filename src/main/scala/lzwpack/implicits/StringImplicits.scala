package lzwpack.implicits

trait StringImplicits {
  implicit class StringOps(sc: StringContext) {
    def b(_args: Any*): Int = Integer.parseUnsignedInt(sc.parts.mkString, 2)
  }
}
