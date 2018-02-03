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
    def map(f: BitBuffer => BitBuffer): UnpackState = UnpackState(f(buffer), counter)
    def readCode: Option[(UnpackState, Code)] = buffer.readOption(codeSize).map {
      case (bb, code) => (UnpackState(bb, counter + 1), code)
    }
  }

  def combinedBuffer(segment: Segment[BitBuffer, _])(init: BitBuffer = BitBuffer.empty): Segment[BitBuffer, Unit] =
    segment.fold(init)(_ ++ _) flatMapResult { case (_, bb) => Segment(bb) }

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = {
    implicit val tag = Tag("unpack")

    def go(stream: Stream[F, Byte], state: (Int, BitBuffer)): Pull[F, Code, Unit] = state match {
      case (counter, buf) =>
        val bitLength = counter.bitLength
        val bytesToRead = ((bitLength - buf.size) / 8) + 1

        stream.pull.unconsN(bytesToRead).flatMap {
          case Some((bytes, rest)) =>
            val (_, product) = bytes.map(BitBuffer(_)).fold(buf)(_ ++ _).force.run
            val (newBuffer, code) = product.read(bitLength)
            debug(show"${counter.hex}: buffer $product", s"read ${code.bin(bitLength)}", s"emit ${code.hex}", show"buffer $newBuffer")
            Pull.output1(code) >> go(rest, (counter + 1, newBuffer))
          case None =>
            val (_, values) = buf.drain(bitLength)
            Pull.output(Segment.array(values))
        }
    }
    in => go(in, (alphabet.size + 2, BitBuffer.empty)).stream
  }
}