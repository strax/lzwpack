package lzwpack

import java.nio.charset.Charset

import fs2._
import lzwpack.data.BitBuffer


object LZW extends Debugging {
  class CompressionException(cause: String) extends RuntimeException(cause)

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
        val bb = makeBitBuffer(code, dict)
        // println(show"(emit) EOF, out $bb")
        (CompressionState(dict, Bytes.empty), Some(bb))
      case Some(byte) if dict.contains(state.buffered :+ byte) =>
        // println(show"(emit) Skip ${byte.hex}, buffer ${(state.buffered :+ byte)}")
        (CompressionState(dict, state.buffered :+ byte), None)
      case Some(byte) =>
        val nextDict = dict.add(state.buffered :+ byte)
        val code = nextDict.get(state.buffered)
        // println(show"(emit) In ${byte.hex}, coded ${(state.buffered :+ byte)} -> ${nextDict.headIndex}, out ${code}")
        (CompressionState(nextDict, Bytes(byte)), Some(makeBitBuffer(code, dict)))
    }
  }

  private def infer(state: CompressionState, code: Option[Code]): (CompressionState, Option[Bytes]) = code match {
    case None =>
      println(show"(infer) EOF, buffer ${state.buffered}")
      (CompressionState(state.dict, Bytes.empty), None)
    case Some(code) =>
      state.dict.findKey(code) match {
        case None =>
          // Handle cases where the code is not yet inferred by the decoder;
          // if the previous value is xω then we can infer the next code to be xωx
          val i = state.dict.nextIndex
          // println(show"(infer) Lookup $i -> ?")
          val inferred = state.buffered ++ state.buffered.take(1)
          // println(show"(infer) Inferred $i <- $inferred")
          // println(show"(infer) Out $inferred")
          (CompressionState(state.dict.add(inferred), inferred), Some(inferred))
        case Some(block) =>
          if (!state.buffered.isEmpty) {
            // New dictionary entry is conjecture + first byte of the current key
            // println(show"(infer) Lookup ${code} -> $block")
            // println(show"(infer) Inferred ${state.dict.nextIndex} <- ${(state.buffered ++ block.take(1))}")
            // println(show"(infer) Out ${block}")
            (CompressionState(state.dict.add(state.buffered ++ block.take(1)), block), Some(block))
          } else {
            // println(show"(infer) Out ${block}")
            (CompressionState(state.dict, block), Some(block))
          }
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
          f(state, None) match {
            case (state, output) =>
              // pprint.pprintln(state, height = Int.MaxValue)
              output.fold(Pull.done.covaryOutput[O])(Pull.output1(_))
          }
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