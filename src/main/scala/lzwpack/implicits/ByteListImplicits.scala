package lzwpack.implicits

import java.nio.charset.{Charset, CodingErrorAction}

import cats.Show

trait ByteListImplicits {
  implicit class ByteListOps(bs: List[Byte]) {
    def unsigned: List[Int] = bs map (_.unsigned)
    def asString(implicit charset: Charset): String = {
      val decoder = charset
        .newDecoder
        .replaceWith(".")
        .onUnmappableCharacter(CodingErrorAction.REPLACE)
        .onMalformedInput(CodingErrorAction.REPLACE)
      val buffer = java.nio.ByteBuffer.wrap(bs.toArray)
      decoder.decode(buffer).toString
    }
  }

  implicit val byteListShow: Show[List[Byte]] = bs => bs.map(_ formatted "%02X").mkString(" ")
}
