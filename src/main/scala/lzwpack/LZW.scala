package lzwpack

import cats._
import cats.data._

import Implicits._

import fs2._

object LZW {
  // A code represents a reference to some known input sequence.
  type Code = Int

  case class Dict[A](entries: Map[A, Code], headIndex: Int) {
    def contains(a: A): Boolean = entries.contains(a)
    def add(a: A): Dict[A] = {
      val newIndex = headIndex + 1
      Dict(entries + (a -> newIndex), newIndex)
    }
    def get(a: A): Int = entries(a)
  }
  object Dict {
    def empty[A] = Dict(Map.empty[A, Code], 0)
    def init[A](alphabet: Alphabet[A]): Dict[A] = alphabet.foldLeft(empty[A])((z, a) => z.add(a))
  }

  type Alphabet[A] = List[A]
  object Alphabet {
    def apply[A](seq: Seq[A]) = seq.toList
    def from(s: String): Alphabet[Char] = s.toList

    implicit val Alphanumeric: Alphabet[Char] = (('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')).toList
    implicit val AllChars: Alphabet[Char] = (Char.MinValue to Char.MaxValue).toList
    // Instead of bytes (8-bit signed integers), we represent byte values by their unsigned int (32-bit) representation.
    implicit val AllBytes: Alphabet[Int] = (0 to 255).toList
  }
  implicit class AlphabetOps[A](a: Alphabet[A]) {
    /**
      * Lifts this alphabet into an alphabet of applicative functors.
      */
    def pure[F[_]](implicit F: Applicative[F]): Alphabet[F[A]] = a.map(F.pure)
  }

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
    in => go(in, "", Dict.init[String](alphabet.map(_.toString))).stream
  }
}