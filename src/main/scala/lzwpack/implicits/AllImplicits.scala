package lzwpack.implicits

import java.nio.charset.Charset

trait AllImplicits extends ByteImplicits with ByteListImplicits with LongImplicits with StringImplicits {
  implicit lazy val ASCII: Charset = Charset.forName("US-ASCII")
}
