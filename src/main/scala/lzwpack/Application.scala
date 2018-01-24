package lzwpack

import cats.effect.IO

object Application {
  import LZW._

  val TestData: String = "TOBEORNOTTOBEORTOBEORNOT"

  def main(args: Array[String]): Unit = {
    println(s"input: ${TestData}")
    val vec = fs2.Stream
      .emits(TestData)
      .through(compress(Alphabet(TestData)))
      .covary[IO]
      .compile
      .toVector.unsafeRunSync()
    println(vec)
  }
}
