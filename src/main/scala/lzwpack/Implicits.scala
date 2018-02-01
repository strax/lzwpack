package lzwpack

import java.lang.{Byte => JavaByte}
import java.nio.charset.{Charset, CodingErrorAction}

import cats._
import cats.implicits._

import scala.io.Codec

/**
  * Contains extension methods to existing types, grouped into an object
  * so all the implicits can be imported with <code>import Implicits._</code>.
  */
trait Implicits {
  implicit lazy val ASCII: Charset = Charset.forName("US-ASCII")

  implicit class ByteOps(b: Byte) {
    /**
      * Converts a signed JVM byte into its unsigned representation.
      */
    def unsigned: Int = JavaByte.toUnsignedInt(b)
  }

  implicit class IntOps(n: Int) {
    /**
      * Returns the bit count (from the most significant bit) of a given unsigned 32-bit integer.
      */
    def bitLength: Int = if(n == 0) 0 else Math.floor(Math.log(n) / Math.log(2)).toInt + 1

    def bin: String = bin(JavaByte.SIZE)
    def bin(size: Int): String = n.toBinaryString.reverse.padTo(size, "0").reverse.mkString

    def hex: String = n formatted "0x%02X"
  }

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

  implicit class StringOps(sc: StringContext) {
    def b(_args: Any*): Int = Integer.parseUnsignedInt(sc.parts.mkString, 2)
  }
}
