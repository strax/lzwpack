package lzwpack

import cats._
import fs2._

object LZW {
  /**
    * A {@see Code} represents the outputted code for a given input subsequence.
    */
  type Code = Int

  /**
    * A dictionary contains unique codes for given chunks in the input
    *
    * @todo Change Map to custom trie
    */
  case class Dict[A](entries: Map[A, Code], headIndex: Int) {
    /**
      * Returns a boolean determining if this dictionary contains a given chunk
      */
    def contains(a: A): Boolean = entries.contains(a)

    /**
      * Adds a new chunk to the dictionary
      * @return a new dictionary with the inserted element
      */
    def add(a: A): Dict[A] = {
      val newIndex = headIndex + 1
      Dict(entries + (a -> newIndex), newIndex)
    }

    /**
      * Returns the code associated with the given chunk.
      * @throws java.util.NoSuchElementException if this dictionary does not contain the given chunk
      */
    def get(a: A): Int = entries(a)
  }

  /**
    * Companion object for {@see Dict}
    */
  object Dict {
    def empty[A] = Dict(Map.empty[A, Code], 0)

    /**
      * Initializes a new dictionary with the given alphabet.
      * In LZW, the dictionary always contains codes for the whole alphabet before compression / decompression.
      */
    def init[A](alphabet: Alphabet[A]): Dict[A] = alphabet.foldLeft(empty[A])((z, a) => z.add(a))
  }

  /**
    * An alphabet for a given input S is the set of possible members an input sequence can be composed from.
    *
    * @todo Implement Set
    */
  type Alphabet[A] = Set[A]

  /**
    * Companion object for {@see Alphabet}.
    */
  object Alphabet {
    /**
      * Returns a new alphabet of type {@tparam A} from an input sequence
      */
    def apply[A](seq: Seq[A]) = seq.toSet

    implicit val Alphanumeric: Alphabet[Char] = Alphabet(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z'))
    implicit val AllChars: Alphabet[Char] = Alphabet(Char.MinValue to Char.MaxValue)
    // Instead of bytes (8-bit signed integers), we represent byte values by their unsigned int (32-bit) representation.
    implicit val AllBytes: Alphabet[Int] = Alphabet(0 to 255)
  }

  /**
    * Extension methods for {@see Alphabet}.
    */
  implicit class AlphabetOps[A](a: Alphabet[A]) {
    /**
      * Lifts this alphabet into an alphabet of {@tparam F}s that are applicative functors.
      */
    def pure[F[_]](implicit F: Applicative[F]): Alphabet[F[A]] = a.map(F.pure)
  }

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
}