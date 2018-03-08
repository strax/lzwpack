package lzwpack

import java.nio.file.{Path, Paths}

import cats.effect.{IO, Sync}
import fs2.{Sink, Stream}

object Application {
  sealed trait OperationMode
  object OperationMode {
    case object Compress extends OperationMode
    case object Decompress extends OperationMode
  }

  case class Options(path: Path = Paths.get("."), mode: OperationMode = OperationMode.Compress, maxCodeSize: Int = MaxCodeSize)

  def options = new scopt.OptionParser[Options]("lzwpack") {
    head("lzwpack", "LZW compression utility")

    opt[Unit]('d', "decompress") action { (_, opts) =>
      opts.copy(mode = OperationMode.Decompress)
    } text "decompress input file instead of compression"

    opt[Int]('b', "bits") action { (n, opts) => opts.copy(maxCodeSize = n) } validate { n =>
      if (n >= 9 && n <= 16) success
      else failure("Code size must be between 9 and 16")
    } text "the maximum number of bits to use for each code"

    arg[String]("path") action { (path, opts) => opts.copy(path = Paths.get(path)) }

    help("help").text("prints this usage text")
  }

  // Use compress(1) compatible alphabet by default
  import Alphabet.Compress

  def inputStream(path: Path): Stream[IO, Byte] = fs2.io.file.readAll[IO](path, 32768)

  def runCompress(path: Path, maxCodeSize: Int, sink: Sink[IO, Byte] = fs2.io.stdout): IO[Unit] = {
    inputStream(path)
      .through(compressAdaptive)
      .through(CompressHeader.encode)
      .to(sink)
      .compile
      .drain
  }

  def runDecompress(path: Path, maxCodeSize: Int, sink: Sink[IO, Byte] = fs2.io.stdout): IO[Unit] = {
    inputStream(path)
      .through(CompressHeader.decode)
      .through(decompressAdaptive)
      .to(sink)
      .compile
      .drain
  }

  def parseOptions(args: Seq[String]): IO[Option[Options]] = IO {
    options.parse(args, Options())
  }

  /**
    * Parses program options and evaluates the corresponding action.
    * This function does not actually perform side effects; it just returns a IO instance that can then
    * be evaluated "at the end of the universe" – the program's [[main]] method.
    */
  def program(args: Seq[String]): IO[Unit] = parseOptions(args) flatMap {
    case Some(Options(path, OperationMode.Compress, maxCodeSize)) => runCompress(path, maxCodeSize)
    case Some(Options(path, OperationMode.Decompress, maxCodeSize)) => runDecompress(path, maxCodeSize)
    case None => IO.unit
  }

  def main(args: Array[String]): Unit = program(args).unsafeRunSync()
}