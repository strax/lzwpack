package lzwpack

import fs2._
import lzwpack.data._
import lzwpack.data.{BitBuffer, ListVector}
import cats.implicits._

object LZW extends Debugging {
  class CompressionException(cause: String) extends RuntimeException(cause)

  type Output = Code

  case class CompressionState(dict: Dict[Bytes], buffered: Bytes)

  private def makeBitBuffer(code: Code, dict: Dict[_]) = BitBuffer(code, dict.currentCode.bitsize)

  /**
    * Processes the given block (given as head and tail) and returns a tuple of a potentially changed
    * dictionary and an Option indicating whether to emit a code to the output.
    *
    * @todo Refactor: can remove Option from output type?
    */
  private def emit(state: CompressionState, head: Option[Byte]): (CompressionState, Segment[BitBuffer, Unit]) = {
    val dict = state.dict

    head match {
      case None =>
        val code = dict.get(state.buffered)
        val bb = makeBitBuffer(code, dict)
        (CompressionState(dict, Bytes.empty), Segment(bb))

      case Some(byte) if dict.contains(state.buffered :+ byte) =>
        (CompressionState(dict, state.buffered :+ byte), Segment.empty)

      case Some(byte) =>
        val nextDict = dict.add(state.buffered :+ byte)
        val code = nextDict.get(state.buffered)
        (CompressionState(nextDict, Bytes(byte)), Segment(makeBitBuffer(code, dict)))
    }
  }

  private def infer(state: CompressionState, code: Option[Code]): (CompressionState, Segment[Byte, Unit]) = code match {
    case None =>
      (CompressionState(state.dict, Bytes.empty), Segment.empty)

    case Some(code) =>
      state.dict.find(code) match {
        case None =>
          // Handle cases where the code is not yet inferred by the decoder;
          // if the previous value is xω then we can infer the next code to be xωx
          val inferred = state.buffered |+| state.buffered.take(1)
          (CompressionState(state.dict.add(inferred), inferred), Segment.seq(inferred))

        case Some(block) =>
          if (!state.buffered.isEmpty) {
            // New dictionary entry is conjecture + first byte of the current key
            (CompressionState(state.dict.add(state.buffered |+| block.take(1)), block), Segment.seq(block))
          } else {
            (CompressionState(state.dict, block), Segment.seq(block))
          }
      }
  }

  type CodecF[I, O] = (CompressionState, Option[I]) => (CompressionState, Segment[O, Unit])

  private def codec[DictT[_]: MakeDict, I, O](f: => CodecF[I, O])(implicit alphabet: Alphabet[Byte]): Pipe[Pure, I, O] = {
    def go(stream: Stream[Pure, I], s0: CompressionState): Pull[Pure, O, Unit] = {
      stream.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          f(s0, Some(head)) match {
            case (s1, segment) => Pull.segment(segment) >> go(tail, s1)
          }

        case None =>
          f(s0, None) match {
            case (_, segment) =>
              Pull.segment(segment)
          }
      }
    }
    stream => go(stream, CompressionState(makeDict[DictT], Bytes.empty)).stream
  }

  def makeDict[T[_]: MakeDict](implicit alphabet: Alphabet[Byte]) =
    implicitly[MakeDict[T]].fromAlphabet(alphabet.pure[ListVector])

  def compress[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, BitBuffer] =
    codec[CompressionDict, Byte, BitBuffer](emit)
  def decompress[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Code, Byte] =
    codec[DecompressionDict, Code, Byte](infer)
}