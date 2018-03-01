package lzwpack

import java.nio.charset.Charset

import fs2._
import lzwpack.data.{BitBuffer, ListVector}

import scala.reflect.ClassTag


object LZW extends Debugging {
  class CompressionException(cause: String) extends RuntimeException(cause)

  import cats._
  import cats.implicits._

  type Output = Code

  case class CompressionState(dict: Dict[Bytes], buffered: Bytes)

  private def makeBitBuffer(code: Code, dict: Dict[_]) = BitBuffer(code, dict.currentCode.bitsize)

  /**
    * Processes the given block (given as head and tail) and returns a tuple of a potentially changed
    * dictionary and an Option indicating whether to emit a code to the output.
    *
    * @todo Refactor: can remove Option from output type?
    */
  private def emit(state: CompressionState, head: Option[Byte]): (CompressionState, Option[BitBuffer]) = {
    val dict = state.dict

    head match {
      case None =>
        val code = dict.get(state.buffered)
        val bb = makeBitBuffer(code, dict)
        (CompressionState(dict, Bytes.empty), Some(bb))

      case Some(byte) if dict.contains(state.buffered :+ byte) =>
        (CompressionState(dict, state.buffered :+ byte), None)

      case Some(byte) =>
        val nextDict = dict.add(state.buffered :+ byte)
        val code = nextDict.get(state.buffered)
        (CompressionState(nextDict, Bytes(byte)), Some(makeBitBuffer(code, dict)))
    }
  }

  private def infer(state: CompressionState, code: Option[Code]): (CompressionState, Option[Bytes]) = code match {
    case None =>
      (CompressionState(state.dict, Bytes.empty), None)

    case Some(code) =>
      state.dict.find(code) match {
        case None =>
          // Handle cases where the code is not yet inferred by the decoder;
          // if the previous value is xω then we can infer the next code to be xωx
          val inferred = state.buffered |+| state.buffered.take(1)
          (CompressionState(state.dict.add(inferred), inferred), Some(inferred))

        case Some(block) =>
          if (!state.buffered.isEmpty) {
            // New dictionary entry is conjecture + first byte of the current key
            (CompressionState(state.dict.add(state.buffered |+| block.take(1)), block), Some(block))
          } else {
            (CompressionState(state.dict, block), Some(block))
          }
      }
  }

  type CodecF[I, O] = (CompressionState, Option[I]) => (CompressionState, Option[O])

  private def codec[DictT[_]: MakeDict, F[_], I, O](f: => CodecF[I, O])(implicit alphabet: Alphabet[Byte]): Pipe[F, I, O] = {
    def go(stream: Stream[F, I], state: CompressionState): Pull[F, O, Unit] = {
      stream.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          f(state, Some(head)) match {
            case (s, Some(output)) =>
              Pull.output(Segment(output)) >> go(tail, s)

            case (s, None) =>
              go(tail, s)
          }

        case None =>
          f(state, None) match {
            case (_, output) =>
              output.fold(Pull.done.covaryOutput[O])(Pull.output1(_))
          }
      }
    }
    stream => go(stream, CompressionState(makeDict[DictT], Bytes.empty)).stream
  }

  def flatten[F[_]](s: Stream[F, Bytes]): Stream[F, Byte] = s.flatMap(bs => Stream.emits(bs.toList))

  def makeDict[T[_]: MakeDict](implicit alphabet: Alphabet[Byte]) =
    implicitly[MakeDict[T]].fromAlphabet(alphabet.pure[ListVector])

  def compress[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, BitBuffer] =
    codec[CompressionDict, F, Byte, BitBuffer](emit)
  def decompress[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Code, Byte] =
    codec[DecompressionDict, F, Code, Bytes](infer).andThen(flatten)
}