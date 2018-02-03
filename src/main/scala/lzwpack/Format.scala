package lzwpack

import fs2.{Chunk, Pipe, Pull, Segment, Stream}
import cats.implicits._
import lzwpack.data.BitBuffer

/**
  * Packs a stream of bits into a byte sequence so that a byte can contain multiple bit sequences.
  */
object Format extends Debugging {
  /**
    * Returns a new {@see Pipe} that packs input tuples of code and its binary length in bits, across byte boundaries.
    */
  def pack[F[_]]: Pipe[F, (Code, Int), Byte] = {
    in => in.map(BitBuffer.tupled).scanSegments(BitBuffer.empty) {
      case (buffer, segment) =>
        segment.flatMapAccumulate(buffer) {
          case (a, b) => (a |+| b).drain(8) match {
            case (buffer_, bytes) => Segment.array(bytes.map(_.toByte)).asResult(buffer_)
          }
        } mapResult (_._2)
    }
  }

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = {
    implicit val tag = Tag("unpack")

    def go(stream: Stream[F, Byte], state: (Int, BitBuffer)): Pull[F, Code, Unit] = state match {
      case (counter, buf) =>
        val bitLength = counter.bitLength
        val bytesToRead = ((bitLength - buf.size) / 8) + 1

        stream.pull.unconsN(bytesToRead).flatMap {
          case Some((bytes, rest)) =>
            val (_, product) = bytes.fold(buf)((acc, b) => acc append BitBuffer(b)).force.run
            val (code, newBuffer) = product.read(bitLength)
            debug(show"${counter.hex}: buffer $product", s"read ${code.bin(bitLength)}", s"emit ${code.hex}", show"buffer $newBuffer")
            Pull.output1(code) >> go(rest, (counter + 1, newBuffer))
          case None =>
            val (_, values) = buf.drain(bitLength)
            for {
              value <- values
            } yield {
              debug("read EOF", s"emit ${value.hex}")
            }

            Pull.outputChunk(Chunk.array(values)) >> Pull.done
        }
    }
    in => go(in, (alphabet.size + 2, BitBuffer.empty)).stream
  }
}