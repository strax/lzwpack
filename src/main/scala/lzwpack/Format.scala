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
  val MaxCodeSize = 12 // bits

  /**
    * Returns a new {@see Pipe} that packs input tuples of code and its binary length in bits, across byte boundaries.
    */
  def pack[F[_]]: Pipe[F, (Code, Int), Byte] = stream => {
    stream.scanSegments(BitBuffer.empty) { case (buffer, segment) =>
      segment.map(BitBuffer.tupled).fold(buffer)(_ ++ _) flatMapResult { case (_, buffer_) =>
        val (rest, bytes) = buffer_.drain(8)
        Segment.array(bytes.map(_.toByte)).asResult(rest)
      }
    }
  }

  case class UnpackState(buffer: BitBuffer, counter: Int) {
    def codeSize: Int = counter.bitLength
    def run(f: UnpackState => BitBuffer): UnpackState = UnpackState(f(this), counter)
    def readCode: Option[(UnpackState, Code)] = buffer.readOption(codeSize).map {
      case (bb, code) => (UnpackState(bb, counter + 1), code)
    }
  }

  def combinedBuffer(segment: Segment[BitBuffer, _])(init: BitBuffer = BitBuffer.empty): Segment[Nothing, BitBuffer] =
    segment.fold(init)(_ ++ _).mapResult(_._2)

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = stream => {
    stream.map(BitBuffer(_)).scanSegments(UnpackState(BitBuffer.empty, alphabet.size + 2)) { case (state, segment) =>
        def drain(acc: Segment[Code, UnpackState]): Segment[Code, UnpackState] = {
          acc flatMapResult { state =>
            state.readCode match {
              case Some((nextState, code)) => drain(Segment(code).asResult(nextState))
              case None => Segment.pure(state)
            }
          }
        }
        combinedBuffer(segment)(state.buffer) flatMapResult (bb => drain(Segment.pure(UnpackState(bb, state.counter))))
    }
  }
}