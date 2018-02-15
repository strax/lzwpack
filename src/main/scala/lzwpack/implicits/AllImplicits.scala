package lzwpack.implicits

import java.nio.charset.Charset

/**
  * Mixins all implicits into a single trait.
  */
trait AllImplicits extends ByteImplicits with ByteListImplicits with LongImplicits with StringImplicits {
  implicit lazy val ASCII: Charset = Charset.forName("US-ASCII")
}
