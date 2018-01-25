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
  def emit(head: Char, tail: Block, dict: Dict[Block]): (Dict[Block], Option[Code]) = {
    if (dict.contains(tail :+ head)) {
      (dict, None)
    } else {
      (dict.add(tail :+ head), Some(dict.get(tail)))
    }
  }

  def infer(code: Code, conjecture: Block, dict: Dict[Block]): (Dict[Block], Block) = {
    val block = dict.reverseGet(code).get
    if (!conjecture.isEmpty) {
      println(s"Adding ${conjecture ++ block.take(1)} to dictionary")
      (dict.add(conjecture ++ block.take(1)), block)
    } else {
      (dict, block)
    }
  }

  def compress[F[_]](implicit alphabet: Alphabet[Char]): Pipe[F, Char, Code] = {
    def go(in: Stream[F, Char], buffer: Block, dict: Dict[Block]): Pull[F, Int, Dict[Block]] = {
      in.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          emit(head, buffer, dict) match {
            case (dict, Some(code)) => {
              println(s"Emitting code $code for $buffer + $head")
              Pull.output1(code) >> go(tail, List(head), dict)
            }
            case (dict, None) => {
              println(s"'${buffer :+ head}' was found in dict, next tail is '${buffer :+ head}'")
              go(tail, buffer :+ head, dict)
            }
          }
        case None =>
          // Output the code for the current buffer
          Pull.output(Segment(dict.get(buffer), 0)) >> Pull.pure(dict)
      }
    }
    println(s"Using alphabet ${alphabet}")
    in => go(in, Nil, Dict.init(alphabet.pure[List])).stream
  }

  def decompress[F[_]](maxBitsPerCode: Int = 12)(implicit alphabet: Alphabet[Char]): Pipe[F, Code, Char] = {
    def go(in: Stream[F, Code], conjecture: Block, dict: Dict[Block]): Pull[F, Char, Unit] = {
      in.pull.uncons1.flatMap {
        case Some((0, _)) => Pull.done
        case Some((code, tail)) =>
          infer(code, conjecture, dict) match {
            case (dict, block) => {
              println(s"Solved block '$block' for code $code with $conjecture")
              Pull.output(Segment.seq(block)) >> go(tail, block, dict)
            }
          }
        case None =>
          Pull.done
      }
    }
    in => go(in, Nil, Dict.init[Block](alphabet.pure[List])).stream
  }
}