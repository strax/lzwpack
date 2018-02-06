package lzwpack

import java.nio.file.{Paths, StandardOpenOption}

import fs2.io.file

class IntegrationTest extends UnitSpec with Fixtures {
  import Alphabet.Compress

  val CorpusPath = Paths.get("fixtures/alice29.txt")

  describe("Tobe fixture") {
    it("produces binary-compatible compression output with the compress tool") {
      val benchmark = TobeCompress
        .through(fs2.hash.md5)
        .compile
        .toVector
        .unsafeRunSync()

      val actual = Tobe
        .through(compressAdaptive)
        .through(CompressHeader.encode)
        .buffer(4)
        .through(fs2.hash.md5)
        .compile
        .toVector
        .unsafeRunSync()

      assert(benchmark == actual)
    }
  }

  describe("Alice29 fixture") {
    it("produces binary-compatible compression output with the compress tool") {
      val benchmark = Alice29Compress
        .through(fs2.hash.md5)
        .compile
        .toVector
        .unsafeRunSync()

      val actual = Alice29
        .through(compressAdaptive)
        .through(CompressHeader.encode)
        .through(fs2.hash.md5)
        .compile
        .toVector
        .unsafeRunSync()

      assert(benchmark == actual)
    }

    it("equals to identity function when compressed and decompressed") {
      val benchmark = Alice29
        .through(fs2.hash.md5)
        .compile
        .toVector
        .unsafeRunSync()

      val actual = Alice29
        .through(compressAdaptive)
        .through(CompressHeader.encode)
        .through(CompressHeader.decode)
        .through(decompressAdaptive)
        .through(fs2.hash.md5)
        .compile
        .toVector
        .unsafeRunSync()

      assert(benchmark == actual)
    }
  }
}
