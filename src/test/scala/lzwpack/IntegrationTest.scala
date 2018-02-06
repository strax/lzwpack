package lzwpack

import java.nio.file.Paths

import fs2.io.file

class IntegrationTest extends UnitSpec with Fixtures {
  import Alphabet.Compress

  val CorpusPath = Paths.get("fixtures/alice29.txt")

  describe("compression and decompression") {
    it("equals to the identity function") {
      val md5a = TestInput
        .compile
        .toVector
        .unsafeRunSync()

      val md5b = TestInput
        .through(compressAdaptive)
        .buffer(2)
        .through(decompressAdaptive)
        .compile
        .toVector
        .unsafeRunSync()

      assert(md5a == md5b)
    }
  }
}
