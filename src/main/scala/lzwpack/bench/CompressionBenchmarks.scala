package lzwpack.bench

import java.nio.file.{Path, Paths}
import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.{ContextShift, IO, IOApp}
import fs2.Sink
import lzwpack._
import org.openjdk.jmh.annotations._

import scala.concurrent.ExecutionContext

// jmh:run -i 10 -wi 10 -f1 -t1 lzwpack.bench.*
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
class CompressionBenchmarks {
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val ec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val sink: Sink[IO, Byte] = fs2.io.file.writeAll(Paths.get("/dev/null"), ec)

  import Alphabet.Compress

  def benchmarkCompress(fixture: Path): IO[Unit] = {
    Application.runCompress(fixture, 16, sink)
  }

  def benchmarkDecompress(fixture: Path): IO[Unit] = {
    Application.runCompress(fixture, 16, sink)
  }

  // Alice29 = 149KB
  @Benchmark def compressAlice29(): Unit = {
    benchmarkCompress(Paths.get("fixtures/alice29.txt")).unsafeRunSync()
  }

  @Benchmark def compressAlice29WithoutPack(): Unit = {
    fs2.io.file.readAll[IO](Paths.get("fixtures/alice29.txt"), ec, 4028)
        .through(LZW.compress)
        .compile.drain.unsafeRunSync()
  }

  @Benchmark def decompressAlice29(): Unit = {
    benchmarkDecompress(Paths.get("fixtures/compress/alice29.txt.Z")).unsafeRunSync()
  }

  @Benchmark def unpackAlice29(): Unit = {
    fs2.io.file.readAll[IO](Paths.get("fixtures/alice29.noheader.txt.Z"), ec, 4028)
        .through(Format.unpack)
      .compile.drain.unsafeRunSync()
  }

  // Ptt5 = 501K
  @Benchmark def compressPtt5(): Unit = {
    benchmarkCompress(Paths.get("fixtures/ptt5")).unsafeRunSync()
  }


  @Benchmark def decompressPtt5(): Unit = {
    benchmarkCompress(Paths.get("fixtures/compress/ptt5.Z")).unsafeRunSync()
  }
}
