package lzwpack

import java.nio.file.Paths

import cats.effect.{IO, Sync}

object Application {
  import LZW._
  import BitPacking._

  def main(args: Array[String]): Unit = {
    implicit val F = implicitly[Sync[IO]]
    implicit val alphabet: Alphabet[Byte] = Alphabet.AllBytes

//    fs2.io.file.readAll(Paths.get("input.txt"), 4028)
//      .through(compress(Alphabet.AllBytes))
//      .through(pack)
//      .through(fs2.io.file.writeAll(Paths.get("output.txt")))
//      .compile
//      .drain.unsafeRunSync()

    fs2.io.file.readAll(Paths.get("fixtures/output.txt"), 4028)
      .through(unpack)
      .through(decompress)
      .bufferAll
      .to(fs2.io.stdout)
      .compile
      .drain
      .unsafeRunSync()
  }
}
