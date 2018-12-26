package lzwpack

import fs2.{Chunk, Pipe}
import cats._
import cats.data._
import cats.implicits._
import lzwpack.data.BitBuffer

import scala.annotation.tailrec

/**
  * Packs a stream of bits into a byte sequence so that a byte can contain multiple bit sequences.
  */
object Format {
  /**
    * Returns a new [[fs2.Pipe]] that packs input [[lzwpack.data.BitBuffer]]s across byte boundaries.
    */
  def pack[F[_]]: Pipe[F, BitBuffer, Byte] = stream => {
    stream.scanChunks(BitBuffer.empty) { case (buffer, segment) =>
      val (rest, bytes) = combinedBuffer(segment)(buffer).drain(8)
      (rest, Chunk.array(bytes.map(_.toByte)))
    }
  }

  case class UnpackState(buffer: BitBuffer, counter: Int) {
    def codeSize: Int = Math.min(counter.bitsize, MaxCodeSize)
    def set(bb: BitBuffer) = UnpackState(bb, counter)
  }

  def unpack1(state: UnpackState): Option[(UnpackState, Code)] = {
    state.buffer.readOption(state.codeSize).map { case (bb, code) =>
      (UnpackState(bb, state.counter + 1), code)
    }
  }

/*  def unpackSegment(state: UnpackState): (Option[UnpackState], Code) =
    unpack1(state).fold(Segment.pure[Code, Option[UnpackState]](None)) { case (state_, code) =>
      Segment(code).asResult(Some(state_))
    }*/

  /**
    * Concatenates all BitBuffers from a chunk into the existing buffer.
    */
  def combinedBuffer(chunk: Chunk[BitBuffer])(init: BitBuffer = BitBuffer.empty): BitBuffer =
    init |+| chunk.fold

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = stream => {
    stream
      .buffer(4)
      .map(b => BitBuffer(b))
      .scanChunks(UnpackState(BitBuffer.empty, alphabet.size)) { case (state, chunk) =>
        def consume(s: UnpackState): (UnpackState, Chain[Code]) =
          s.buffer.readOption(s.codeSize) match {
            case Some((rest, code)) => consume(UnpackState(rest, s.counter + 1)).map(Chain(code) ++ _)
            case None => (s, Chain.empty)
          }

        consume(state.set(state.buffer |+| chunk.fold)).map(Chunk.chain(_))
      }
  }
}