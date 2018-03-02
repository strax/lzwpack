package lzwpack

import java.nio.file.Path

import cats.effect.IO
import fs2.Chunk

import scala.io.BufferedSource

object VanillaLZW {
  def readFile(path: String): IO[Iterator[String]] = {
    ???
  }
}
