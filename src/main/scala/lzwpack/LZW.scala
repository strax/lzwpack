package lzwpack

import java.nio.charset.Charset

import fs2._
import lzwpack.data.BitBuffer


object LZW extends Debugging {
  import cats._
  import cats.implicits._

  type Output = Code

  case class CompressionState(dict: Dict[Bytes], buffered: Bytes)

  private def makeBitBuffer(code: Code, dict: Dict[_]) = BitBuffer(code, dict.nextIndex.bitLength)

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
        (CompressionState(dict, Bytes.empty), Some(makeBitBuffer(code, dict)))
      case Some(byte) if dict.contains(state.buffered :+ byte) =>
        (CompressionState(dict, state.buffered :+ byte), None)
      case Some(byte) =>
        val code = dict.get(state.buffered)
        (CompressionState(dict.add(state.buffered :+ byte), Bytes(byte)), Some(makeBitBuffer(code, dict)))
    }
  }

  private def infer(state: CompressionState, code: Option[Code]): (CompressionState, Option[Bytes]) = code match {
    case None => (CompressionState(state.dict, Bytes.empty), None)
    case Some(code) =>
      val block = state.dict.findKey(code).get
      if (!state.buffered.isEmpty) {
        // New dictionary entry is conjecture + first byte of the current key
        (CompressionState(state.dict.add(state.buffered ++ block.take(1)), block), Some(block))
      } else {
        (CompressionState(state.dict, block), Some(block))
      }
  }

  type CodecF[I, O] = (CompressionState, Option[I]) => (CompressionState, Option[O])

  private def codec[F[_], I, O](f: => CodecF[I, O])(implicit alphabet: Alphabet[Byte]): Pipe[F, I, O] = {
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
          f(state, None)._2.fold(Pull.done.covaryOutput[O])(output => {
            Pull.output1(output)
          })
      }
    }
    in => {
      val dict = Dict.init(alphabet.pure[List])
      go(in, CompressionState(dict, Bytes.empty)).stream
    }
  }

  def flatten[F[_], O](s: Stream[F, Seq[O]]) = s.flatMap(bs => Stream.emits(bs))

  def compress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Byte, BitBuffer](emit)
  def decompress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Code, Bytes](infer).andThen(flatten)
}