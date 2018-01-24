package lzwpack

import fs2._
import cats.effect.IO
import cats.instances.string._

class LZWSpec extends UnitSpec {
  import LZW._

  describe("compress") {
    implicit val alphabet: Alphabet[Char] = Alphabet('A' to 'Z')

    it("encodes an input string correctly") {
      val input = Stream.emits("TOBEORNOTTOBEORTOBEORNOT")
      assertResult(List(20, 15, 2, 5, 15, 18, 14, 15, 20, 27, 29, 31, 36, 30, 32, 34, 0))(input.through(compress).toList)
    }

    it("decodes a compressed string correctly") {
      val input = Stream.emits(List(20, 15, 2, 5, 15, 18, 14, 15, 20, 27, 29, 31, 36, 30, 32, 34, 0))
      assertResult("TOBEORNOTTOBEORTOBEORNOT")(input.through(decompress(12)).toList.mkString)
    }
  }
}
