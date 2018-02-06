package lzwpack

import java.nio.file.Paths

import fs2._
import fs2.io.file
import cats.effect.IO

trait Fixtures {
  lazy val Alice29: Stream[IO, Byte] = file.readAll[IO](Paths.get("fixtures/alice29.txt"), 4028)
  lazy val TestInput: Stream[IO, Byte] = file.readAll[IO](Paths.get("fixtures/input.txt"), 4028)
}
