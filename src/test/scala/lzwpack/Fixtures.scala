package lzwpack

import java.nio.file.Paths

import fs2._
import fs2.io.file
import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

trait Fixtures {
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val ec = ExecutionContext.global

  lazy val Alice29: Stream[IO, Byte] = file.readAll[IO](Paths.get("fixtures/alice29.txt"), ec, 32768)
  lazy val Alice29Compress: Stream[IO, Byte] = file.readAll[IO](Paths.get("fixtures/compress/alice29.txt.Z"),ec,  32768)
  lazy val Tobe: Stream[IO, Byte] = file.readAll[IO](Paths.get("fixtures/tobe.txt"), ec, 32768)
  lazy val TobeCompress: Stream[IO, Byte] = file.readAll[IO](Paths.get("fixtures/compress/tobe.txt.Z"),ec, 32768)
}
