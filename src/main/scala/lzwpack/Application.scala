package lzwpack

import java.nio.file.{Path, Paths}

import cats.effect.{IO, Sync}
import fs2.Stream

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

  def inputStream[F[_]: Sync](path: Path): Stream[F, Byte] = fs2.io.file.readAll(path, 4028)

  def runCompress[F[_]: Sync](path: Path, maxCodeSize: Int): F[Unit] = {
    inputStream(path)
      .through(compressAdaptive)
      .through(CompressHeader.encode)
      .to(fs2.io.stdout)
      .compile
      .drain
  }

  def runDecompress[F[_]: Sync](path: Path, maxCodeSize: Int): F[Unit] = {
    inputStream(path)
      .through(CompressHeader.decode)
      .through(decompressAdaptive)
      .to(fs2.io.stdout)
      .compile
      .drain
  }

  /**
    * Parses program options and evaluates the corresponding action.
    * This function does not actually perform side effects; it just returns a [[F]] instance that can then
    * be evaluated "at the end of the universe" – the program's [[main]] method.
    */
  def program[F[_]: Sync](args: Array[String]): F[Unit] = options.parse(args, Options()) match {
    case Some(Options(path, OperationMode.Compress, maxCodeSize)) => runCompress(path, maxCodeSize)
    case Some(Options(path, OperationMode.Decompress, maxCodeSize)) => runDecompress(path, maxCodeSize)
    case None => Sync[F].unit
  }

  def main(args: Array[String]): Unit = program[IO](args).unsafeRunSync()
}
