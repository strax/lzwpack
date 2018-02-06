package lzwpack

import java.nio.file.Paths

import cats.effect.{IO, Sync}

object Application {
  import LZW._
  import Format._

  def main(args: Array[String]): Unit = {
    import Alphabet.Compress

    implicit val F = implicitly[Sync[IO]]

//    fs2.io.file.readAll(Paths.get("fixtures/input.txt"), 4028)
//      .through(compress)
//      .through(pack)
//      .through(fs2.io.file.writeAll(Paths.get("fixtures/output.txt")))
//      .compile
//      .drain.unsafeRunSync()

    fs2.io.file.readAll(Paths.get("fixtures/output.txt"), 3)
      .through(unpack)
      .through(decompress)
      .bufferAll
      .to(fs2.io.stdout)
      .compile
      .drain
      .unsafeRunSync()
  }
}
