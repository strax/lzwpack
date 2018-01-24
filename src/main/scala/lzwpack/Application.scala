package lzwpack

import Implicits._
import cats.effect.IO
import fs2.Chunk

object Application {
  import LZW._

  val TestData: String = "TOBEORNOTTOBEORTOBEORNOT"

  def main(args: Array[String]): Unit = {
    println(s"input: ${TestData}")
    val vec = fs2.Stream
      .emits(TestData)
      .through(compress(('A' to 'Z').toList))
      .covary[IO]
      .compile
      .toVector.unsafeRunSync()
    println(vec)
  }
}
