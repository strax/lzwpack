package lzwpack

import cats._
import cats.data._
import cats.implicits._

object LZW {
  case class CompressionState(tail: List[Char], dictionary: Map[Seq[Char], Int], i: Int)

  object CompressionState {
    val ByteAlphabet: Seq[Char] = ('A' to 'Z')
    def initialDict[A](alphabet: Seq[A]) = alphabet.map(Seq(_)).zip(Stream.from(1)).toMap
    def empty = CompressionState(Nil, initialDict(ByteAlphabet), 28)
  }

  def process(state: CompressionState, head: Char): (CompressionState, Int) = state match {
    case CompressionState(tail, dictionary, i) =>
      val block = head :: tail
      println(s"Current chunk to check: ${block.mkString}")
      if (dictionary.contains(head :: tail)) {
        (CompressionState(head :: tail, dictionary, i), 0)
      } else {
        println(s"Unindexed chunk ${block.mkString}")
        (CompressionState(List(head), dictionary.updated(head :: tail, i), i+1), i.toByte)
      }
  }

  def compress(input: Stream[Char]): (CompressionState, Stream[Int]) =
    input.traverse(h => State((s: CompressionState) => process(s, h))).run(CompressionState.empty).value
}