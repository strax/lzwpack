package lzwpack

import fs2._
import cats._
import cats.implicits._

object LZW {
  type Block = List[Char]

  /**
    * Processes the given block (given as head and tail) and returns a tuple of a potentially changed
    * dictionary and an Option indicating whether to emit a code to the output.
    */
  private def emit(head: Char, tail: Block, dict: Dict[Block]): (Dict[Block], Option[List[Code]], Block) = {
    println(s"Input is ${head.toInt}")
    if (head == 0) return (dict, Some(List(dict.get(tail), 0)), List())
    if (dict.contains(tail :+ head)) {
      (dict, None, tail :+ head)
    } else {
      (dict.add(tail :+ head), Some(List(dict.get(tail))), List(head))
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

  private def codec[F[_], I, O](f: CodecF[I, O])(implicit alphabet: Alphabet[Char], N: Numeric[I]): Pipe[F, I, O] = {
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

  def compress[F[_]](implicit alphabet: Alphabet[Char]) = codec[F, Char, Code](emit)
  def decompress[F[_]](implicit alphabet: Alphabet[Char]) = codec[F, Code, Char](infer)
}