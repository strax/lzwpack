package lzwpack

import java.nio.ByteBuffer
import java.nio.charset.Charset

import scala.io.Codec
import Implicits._

object Application {
  import LZW._

  val TestData: Stream[Char] = "DEAD BEEF IS THE BESTEST BECAUSE DETOX".toCharArray.toStream

  def bytesToString(bs: Seq[Byte]) = Codec.ISO8859.decoder.decode(ByteBuffer.wrap(bs.toArray))

  def main(args: Array[String]): Unit = {
    println(s"input: ${TestData.mkString}")
    val (state, stream) = compress(TestData)
    println(state.dictionary.map { case (k, v) => (k.mkString, v) })
    stream.print()
  }
}
