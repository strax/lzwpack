package lzwpack

import cats.data.{State, StateT}
import fs2.{Chunk, Pipe, Pull, Segment, Stream}
import cats.implicits._
import cats.kernel.Monoid
import lzwpack.data.BitBuffer

/**
  * Packs a stream of bits into a byte sequence so that a byte can contain multiple bit sequences.
  */
object Format extends Debugging {
  /**
    * Returns a new [[Pipe]] that packs input [[BitBuffer]]s across byte boundaries.
    */
  def pack[F[_]]: Pipe[F, BitBuffer, Byte] = stream => {
    stream.scanSegments(BitBuffer.empty) { case (buffer, segment) =>
      combinedBuffer(segment)(buffer) flatMapResult { buffer =>
        val (rest, bytes) = buffer.drain(8)
        Segment.array(bytes.map(_.toByte)).asResult(rest)
      }
    }
  }

  case class UnpackState(buffer: BitBuffer, counter: Int) {
    def codeSize: Int = Math.min((counter).bitsize, MaxCodeSize)
    def set(bb: BitBuffer) = UnpackState(bb, counter)
  }

  def unpack1(state: UnpackState): Option[(UnpackState, Code)] = {
    state.buffer.readOption(state.codeSize).map { case (bb, code) =>
      (UnpackState(bb, state.counter + 1), code)
    }
  }

  def unpackSegment(state: UnpackState): Segment[Code, Option[UnpackState]] =
    unpack1(state).fold(Segment.pure[Code, Option[UnpackState]](None)) { case (state_, code) =>
      Segment(code).asResult(Some(state_))
    }

  def combinedBuffer(segment: Segment[BitBuffer, _])(init: BitBuffer = BitBuffer.empty): Segment[Nothing, BitBuffer] =
    segment.fold(init)(_ ++ _).mapResult(_._2)

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = stream => {
    stream
      .buffer(4)
      .map(b => BitBuffer(b))
      .scanSegments(UnpackState(BitBuffer.empty, alphabet.size)) { case (state, segment) =>
        def drain(state: UnpackState): Segment[Code, UnpackState] = {
          unpackSegment(state).flatMapResult(_.fold(Segment.pure[Code, UnpackState](state))(drain))
        }

        combinedBuffer(segment)(state.buffer) flatMapResult (bb => drain(state.set(bb)))
      }
  }
}