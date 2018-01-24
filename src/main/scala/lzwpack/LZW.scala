package lzwpack

import fs2._

object LZW {
  /**
    * Processes the given block (given as head and tail) and returns a tuple of a potentially changed
    * dictionary and an Option indicating whether to emit a code to the output.
    *
    * @todo Unwrap Option[(Dict[String], Code)] to (Dict[String], Option[Code])
    */
  def emit(head: Char, tail: String, dict: Dict[String]): Option[(Dict[String], Code)] = {
    if (dict.contains(tail + head)) {
      None
    } else {
      Some(dict.add(tail + head)).map(d => (d, dict.get(tail)))
    }
  }

  def infer(code: Code, conjecture: String, dict: Dict[String]): (Dict[String], String) = {
    val block = dict.reverseGet(code).get
    if (!conjecture.isEmpty) {
      println(s"Adding $conjecture + $block to dictionary")
      (dict.add(conjecture + block.head), block)
    } else {
      (dict, block)
    }
  }

  def compress[F[_]](implicit alphabet: Alphabet[Char]): Pipe[F, Char, Int] = {
    def go(in: Stream[F, Char], buffer: String, dict: Dict[String]): Pull[F, Int, Dict[String]] = {
      in.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          emit(head, buffer, dict) match {
            case Some((dict, code)) => {
              println(s"Emitting code $code for $buffer + $head")
              Pull.output1(code) >> go(tail, head.toString, dict)
            }
            case None => {
              println(s"'${buffer + head}' was found in dict, next tail is '${buffer + head}'")
              go(tail, buffer + head, dict)
            }
          }
        case None =>
          // Output the code for the current buffer
          Pull.output(Segment(dict.get(buffer), 0)) >> Pull.pure(dict)
      }
    }
    println(s"Using alphabet ${alphabet}")
    in => go(in, "", Dict.init[String](alphabet.map(_.toString))).stream
  }

  def decompress[F[_]](maxBitsPerCode: Int = 12)(implicit alphabet: Alphabet[Char]): Pipe[F, Code, String] = {
    def go(in: Stream[F, Code], conjecture: String, dict: Dict[String]): Pull[F, String, Unit] = {
      in.pull.uncons1.flatMap {
        case Some((0, _)) => Pull.done
        case Some((code, tail)) =>
          infer(code, conjecture, dict) match {
            case (dict, block) => {
              println(s"Solved block '$block' for code $code with $conjecture")
              Pull.output1(block) >> go(tail, block, dict)
            }
          }
        case None =>
          Pull.done
      }
    }
    in => go(in, "", Dict.init[String](alphabet.map(_.toString))).stream
  }
}