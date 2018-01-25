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
  def emit(head: Char, tail: Block, dict: Dict[Block]): (Dict[Block], Option[List[Code]], Block) = {
    if (head == 0) return (dict, Some(List(dict.get(tail), 0)), List())
    if (dict.contains(tail :+ head)) {
      (dict, None, tail :+ head)
    } else {
      (dict.add(tail :+ head), Some(List(dict.get(tail))), List(head))
    }
  }

  def infer(code: Code, conjecture: Block, dict: Dict[Block]): (Dict[Block], Option[Block], Block) = {
    if (code == 0) return (dict, None, List())
    val block = dict.reverseGet(code).get
    if (!conjecture.isEmpty) {
      println(s"Adding ${conjecture ++ block.take(1)} to dictionary")
      (dict.add(conjecture ++ block.take(1)), Some(block), block)
    } else {
      (dict, Some(block), block)
    }
  }

  def compress[F[_]](implicit alphabet: Alphabet[Char]): Pipe[F, Char, Code] = {
    def go(in: Stream[F, Char], buffer: Block, dict: Dict[Block]): Pull[F, Int, Unit] = {
      in.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          emit(head, buffer, dict) match {
            case (dict, Some(code), buffer) => {
              println(s"Emitting code $code for $buffer")
              Pull.output(Segment.seq(code)) >> go(tail, buffer, dict)
            }
            case (dict, None, buffer) => {
              println(s"'${buffer}' was found in dict, next tail is '${buffer}'")
              Pull.done >> go(tail, buffer, dict)
            }
          }
        case None =>
          // Send NUL to emitter and output the final result, if any
          emit(0, buffer, dict)._2.fold(Pull.done.covaryOutput[Code])(codes => {
            println(s"Compression ending, remaining block is $codes")
            Pull.output(Segment.seq(codes))
          })
      }
    }
    println(s"Using alphabet ${alphabet}")
    in => go(in, Nil, Dict.init(alphabet.pure[List])).stream
  }

  def decompress[F[_]](maxBitsPerCode: Int = 12)(implicit alphabet: Alphabet[Char]): Pipe[F, Code, Char] = {
    def go(in: Stream[F, Code], conjecture: Block, dict: Dict[Block]): Pull[F, Char, Unit] = {
      in.pull.uncons1.flatMap {
        case Some((code, tail)) =>
          infer(code, conjecture, dict) match {
            case (dict, Some(block), buffer) => {
              println(s"Solved block '$block' for code $code with $buffer")
              Pull.output(Segment.seq(block)) >> go(tail, buffer, dict)
            }
            case (dict, None, buffer) => go(tail, buffer, dict)
          }
        case None =>
          Pull.done
      }
    }
    in => go(in, Nil, Dict.init[Block](alphabet.pure[List])).stream
  }
}