package lzwpack.implicits

import java.nio.charset.{Charset, CodingErrorAction}

import cats.Show

/**
  * Provides extension methods for the [[scala.List[Byte]]] type.
  */
trait ByteListImplicits {
  implicit class ByteListOps(bs: List[Byte]) {
    /**
      * Converts each byte in this byte list into an unsigned int.
      */
    def unsigned: List[Int] = bs map (_.unsigned)

    /**
      * Converts this byte list to a string using the implicit charset in scope.
      */
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
