package lzwpack

import java.nio.charset.Charset

import fs2._


object LZW {
  import cats._
  import cats.implicits._

  type Block = List[Byte]
  type Output = (Code, Int)

  private def debug(input: List[Byte], dict: Dict[Block], indexed: Option[Int], emitted: Int): Unit = {
    val msg = show"${input} \t ${input.asString} \t\t create ${indexed.fold("<none>")(_.hex)} \t\t emit ${emitted.hex} ${emitted.bin(dict.headIndex.bitLength)} (${dict.headIndex.bitLength} bits)"
    System.err.println(msg)
  }

  /**
    * Processes the given block (given as head and tail) and returns a tuple of a potentially changed
    * dictionary and an Option indicating whether to emit a code to the output.
    */
  private def emit(head: Byte, tail: Block, dict: Dict[Block]): (Dict[Block], Option[List[Output]], Block) = {
    val input = tail :+ head

    val emitLength = dict.headIndex.bitLength
    if (head == 0) {
      val code = dict.get(tail)
      debug(input, dict, None, code)
      return (dict, Some(List((code, emitLength), (0, 0))), List())
    }
    if (dict.contains(input)) {
      (dict, None, input)
    } else {
      val code = dict.get(tail)
      debug(input, dict, Some(dict.headIndex + 1), code)
      (dict.add(input), Some(List((code, emitLength))), List(head))
    }
  }

  private def infer(code: Code, conjecture: Block, dict: Dict[Block]): (Dict[Block], Option[Block], Block) = {
    if (code == 0) return (dict, None, List())
    val block = dict.reverseGet(code).get
    if (!conjecture.isEmpty) {
      println(s"Adding ${conjecture ++ block.take(1)} to dictionary")
      (dict.add(conjecture ++ block.take(1)), Some(block), block)
    } else {
      (dict, Some(block), block)
    }
  }

  type CodecF[I, O] = (I, Block, Dict[Block]) => (Dict[Block], Option[List[O]], Block)

  private def codec[F[_], I, O: Show](f: CodecF[I, O])(implicit alphabet: Alphabet[Byte], N: Numeric[I], S: Show[List[O]]): Pipe[F, I, O] = {
    def go(stream: Stream[F, I], buffer: Block, dict: Dict[Block]): Pull[F, O, Unit] = {
      stream.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          f(head, buffer, dict) match {
            case (dict, Some(output), buffer) =>
              Pull.output(Segment.seq(output)) >> go(tail, buffer, dict)
            case (dict, None, buffer) =>
              Pull.done >> go(tail, buffer, dict)
          }
        case None =>
          f(N.zero, buffer, dict)._2.fold(Pull.done.covaryOutput[O])(output => {
            println(show"Compression ending, final block ${S.show(output)}")
            Pull.output(Segment.seq(output))
          })
      }
    }
    in => go(in, Nil, Dict.init(alphabet.pure[List])).stream
  }

  def compress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Byte, (Code, Int)](emit)
  def decompress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Code, Byte](infer)
}