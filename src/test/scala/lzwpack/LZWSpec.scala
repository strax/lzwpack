package lzwpack

import cats.effect.IO
import fs2._

class LZWSpec extends UnitSpec {
  import LZW._

  describe("compress") {
    implicit val alphabet: Alphabet[Byte] = Alphabet('A' to 'Z').map(_.toByte)

    it("encodes an input string correctly") {
      val input = Stream.emits("TOBEORNOTTOBEORTOBEORNOT".getBytes)
      assertResult(List(20, 15, 2, 5, 15, 18, 14, 15, 20, 27, 29, 31, 36, 30, 32, 34, 0))(input.through(compress[Pure]).map(_._1).toList)
    }

    it("decodes a compressed string correctly") {
      val input = Stream.emits(List(20, 15, 2, 5, 15, 18, 14, 15, 20, 27, 29, 31, 36, 30, 32, 34, 0))
      assertResult("TOBEORNOTTOBEORTOBEORNOT")(input.through(decompress[Pure]).toList.mkString)
    }
  }
}
