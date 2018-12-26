package lzwpack

import fs2.{Chunk, Pipe}
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
    stream.scanChunks(BitBuffer.empty) { case (buffer, chunk) =>
      (buffer |+| chunk.fold).drain(8).map(xs => Chunk.seq(xs.map(_.toByte)))
    }
  }

  case class UnpackState(buffer: BitBuffer, counter: Int) {
    def codeSize: Int = Math.min(counter.bitsize, MaxCodeSize)
    def set(bb: BitBuffer) = UnpackState(bb, counter)
  }

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = stream => {
    @tailrec
    def consume(s: UnpackState, acc: Chain[Code]): (UnpackState, Chain[Code]) =
      s.buffer.readOption(s.codeSize) match {
        case Some((rest, code)) => consume(UnpackState(rest, s.counter + 1), acc ++ Chain(code))
        case None => (s, acc)
      }

    stream
      .buffer(4)
      .map(BitBuffer(_))
      .scanChunks(UnpackState(BitBuffer.empty, alphabet.size)) { case (state, chunk) => {
        consume(state.copy(buffer = state.buffer |+| chunk.fold), Chain.empty) map Chunk.chain
      }
    }
  }
}