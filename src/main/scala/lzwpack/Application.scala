package lzwpack

import java.nio.file.Paths

import cats.effect.{IO, Sync}

object Application {
  import LZW._
  import Format._

  def main(args: Array[String]): Unit = {
    import Alphabet.Compress

    implicit val F = implicitly[Sync[IO]]

    fs2.io.file.readAll(Paths.get("fixtures/alice29.txt"), 4028)
      .through(compressAdaptive)
      .to(fs2.io.file.writeAll(Paths.get("tmp/alice29.txt.Z")))
      .compile
      .drain.unsafeRunSync()

    fs2.io.file.readAll(Paths.get("tmp/alice29.txt.Z"), 4028)
      .through(CompressHeader.decode)
      .through(decompressAdaptive)
      .to(fs2.io.file.writeAll(Paths.get("tmp/alice29.txt")))
      .compile
      .drain
      .unsafeRunSync()
  }
}
