package lzwpack

import java.nio.charset.Charset

import fs2._


object LZW {
  import cats._
  import cats.implicits._

  type Output = Code

  case class CompressionState(dict: Dict[Bytes], buffered: Bytes)

  /**
    * Processes the given block (given as head and tail) and returns a tuple of a potentially changed
    * dictionary and an Option indicating whether to emit a code to the output.
    *
    * @todo Refactor: can remove Option from output type?
    */
  private def emit(state: CompressionState, head: Byte): (CompressionState, Option[List[Code]]) = {
    val input = state.buffered :+ head
    val dict = state.dict

    def debug(indexed: Option[Int], emitted: Int): Unit = {
      val msg = show"[emit]\t\tread $input  ${input.asString} \t\t index ${indexed.fold("<none>")(_.hex)} \t\t emit ${emitted.hex} ${emitted.bin(dict.headIndex.bitLength)} (${dict.headIndex.bitLength} bits)"
      System.err.println(msg)
    }

    if (head == 0) {
      val code = dict.get(state.buffered)
      debug(None, code)
      System.err.println(s"[emit]\t\temit ${0.hex}  <EOF>")
      return (CompressionState(dict, Bytes.empty), Some(List(code, 0)))
    }
    if (dict.contains(input)) {
      (CompressionState(dict, input), None)
    } else {
      val code = dict.get(state.buffered)
      debug(Some(dict.nextIndex), code)
      (CompressionState(dict.add(input), Bytes(head)), Some(List(code)))
    }
  }

  private def infer(state: CompressionState, code: Code): (CompressionState, Option[Bytes]) = {
    def debug(indexed: Option[Bytes], emit: Bytes): Unit = {
      val msg = show"[infer]\t\tread ${code.hex}\t\temit $emit  ${emit.asString}\t\tindex ${indexed.fold("<none>")(bs => show"${state.dict.nextIndex.hex}  ${bs.asString}")}"
      System.err.println(msg)
    }

    if (code == 0) return (CompressionState(state.dict, Bytes.empty), None)
    System.err.println(show"[infer]\t\tread ${code.hex}")
    val block = state.dict.reverseGet(code).get
    if (!state.buffered.isEmpty) {
      // println(s"Adding ${state.buffered ++ block.take(1)} to dictionary")
      debug(Some(state.buffered ++ block.take(1)), block)
      (CompressionState(state.dict.add(state.buffered ++ block.take(1)), block), Some(block))
    } else {
      debug(None, block)
      (CompressionState(state.dict, block), Some(block))
    }
  }

  type CodecF[I, O] = (CompressionState, I) => (CompressionState, Option[List[O]])

  private def codec[F[_], I, O](op: String = "codec")(f: => CodecF[I, O])(implicit alphabet: Alphabet[Byte], N: Numeric[I]): Pipe[F, I, O] = {
    def go(stream: Stream[F, I], state: CompressionState): Pull[F, O, Unit] = {
      stream.pull.uncons1.flatMap {
        case Some((head, tail)) =>
          f(state, head) match {
            case (s, Some(output)) =>
              Pull.output(Segment.seq(output)) >> go(tail, s)
            case (s, None) =>
              Pull.done >> go(tail, s)
          }
        case None =>
          f(state, N.zero)._2.fold(Pull.done.covaryOutput[O])(output => {
            // println(show"Compression ending, final block ${S.show(output)}")
            Pull.output(Segment.seq(output))
          })
      }
    }
    in => {
      val dict = Dict.init(alphabet.pure[List])
      System.err.println(show"mode: $op")
      System.err.println(show"using alphabet ${alphabet.head.hex} ${alphabet.head.bin} – ${alphabet.last.unsigned.hex} ${alphabet.last.unsigned.bin} (${alphabet.size} elements)")
      System.err.println(show"code index at ${dict.headIndex.hex}")
      go(in, CompressionState(dict, Bytes.empty)).stream
    }
  }

  private def makeOutput(s: (CompressionState, Option[List[Int]])) = s match {
    case (state, Some(emits)) => (state, Some(emits.map((_, state.dict.codeSize))))
    case (state, None) => (state, None)
  }

  def compress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Byte, (Code, Int)](op = "compress") {
    case (state, input) => makeOutput(emit(state, input))
  }
  def decompress[F[_]](implicit alphabet: Alphabet[Byte]) = codec[F, Code, Byte](op = "decompress")(infer)
}