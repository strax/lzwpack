package lzwpack

import java.nio.charset.Charset

import fs2._


object LZW {
  import cats._
  import cats.implicits._

  type Block = List[Byte]
  type Output = (Code, Int)

  /**
    * Processes the given block (given as head and tail) and returns a tuple of a potentially changed
    * dictionary and an Option indicating whether to emit a code to the output.
    */
  private def emit(head: Byte, tail: Block, dict: Dict[Block]): (Dict[Block], Option[List[Output]], Block) = {
    if (head == 0) return (dict, Some(List((dict.get(tail), dict.headIndex.bitLength), (0, 0))), List())
    val block = tail :+ head
    if (dict.contains(block)) {
      (dict, None, block)
    } else {
      System.err.println(show"${block} \t ${block.asString} \t\t create ${(dict.headIndex + 1).hex} \t\t emit ${(dict.get(tail)).hex} ${dict.get(tail).bin(dict.headIndex.bitLength)} (${dict.headIndex.bitLength} bits)")
      (dict.add(block), Some(List((dict.get(tail), dict.headIndex.bitLength))), List(head))
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

  type CodecF[I, O] = (I, Block, Dict[Block]) => (Dict[Block], Option[Seq[O]], Block)

  private def codec[F[_], I, O](f: CodecF[I, O])(implicit alphabet: Alphabet[Byte], N: Numeric[I]): Pipe[F, I, O] = {
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
            println(s"Compression ending, final block $output")
            Pull.output(Segment.seq(output))
          })
      }
    }
    in => go(in, Nil, Dict.init(alphabet.pure[List])).stream
  }

  def compress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Byte, (Code, Int)](emit)
  def decompress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Code, Byte](infer)
}