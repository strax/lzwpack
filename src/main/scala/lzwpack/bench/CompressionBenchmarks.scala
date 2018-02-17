package lzwpack.bench

import java.nio.file.{Path, Paths}
import java.util.concurrent.TimeUnit

import cats.effect.IO
import fs2.Sink
import lzwpack.Application
import org.openjdk.jmh.annotations._

// jmh:run -i 10 -wi 10 -f1 -t1 lzwpack.bench.*
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
class CompressionBenchmarks {
  val sink: Sink[IO, Byte] = fs2.io.file.writeAll(Paths.get("/dev/null"))

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

  @Benchmark def decompressAlice29(): Unit = {
    benchmarkDecompress(Paths.get("fixtures/compress/alice29.txt.Z")).unsafeRunSync()
  }

  // Ptt5 = 501K
  @Benchmark def compressPtt5(): Unit = {
    benchmarkCompress(Paths.get("fixtures/ptt5")).unsafeRunSync()
  }


  @Benchmark def decompressPtt5(): Unit = {
    benchmarkCompress(Paths.get("fixtures/compress/ptt5.Z")).unsafeRunSync()
  }
}
