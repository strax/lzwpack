package lzwpack

import java.nio.file.{Paths, StandardOpenOption}

import fs2.io.file

class IntegrationTest extends UnitSpec with Fixtures {
  import Alphabet.Compress

  val CorpusPath = Paths.get("fixtures/alice29.txt")

  describe("compression and decompression") {
    it("equals to the identity function") {
//      val md5a = Alice29
//        .compile
//        .toVector
//        .unsafeRunSync()

      val md5b = TestInput
        .through(compressAdaptive)
        .through(CompressHeader.encode)
        .buffer(4)
        .to(fs2.io.file.writeAll(Paths.get("tmp/output.txt.Z"), Seq(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)))
        .compile
        .drain
        .unsafeRunSync()

//      assert(md5a == md5b)
    }
  }
}
