package lzwpack

import java.nio.file.Paths

import cats.effect.{IO, Sync}

object Application {
  import LZW._
  import BitPacking._

  val TestData: Seq[Byte] = "TOBEORNOTTOBEORTOBEORNOT".getBytes()

  def main(args: Array[String]): Unit = {
    println(s"input: ${TestData}")
    implicit val F = implicitly[Sync[IO]]

/*    fs2.io.file.readAll(Paths.get("input.txt"), 4028)
      .through(compress(Alphabet.AllBytes))
      .through(pack)
      .through(fs2.io.file.writeAll(Paths.get("output.txt")))
      .compile
      .drain.unsafeRunSync()*/
  }
}
