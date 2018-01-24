package lzwpack

import Implicits._
import cats.effect.IO
import fs2.Chunk

object Application {
  import LZW._

  val TestData: String = "DEAD BEEF IS THE BESTEST BECAUSE DETOX"

  def main(args: Array[String]): Unit = {
    println(s"input: ${TestData}")
    val vec = fs2.Stream
      .emits(TestData)
      .through(compress(Alphabet.string("ABCDEFGHIJKLMNOPQRSTUVWXYZ ")))
      .covary[IO]
      .compile
      .toVector.unsafeRunSync()
    println(vec)
  }
}
