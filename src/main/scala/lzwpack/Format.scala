package lzwpack

import fs2.{Chunk, Pipe, Pull, Segment, Stream}
import cats._
import cats.data._
import cats.implicits._
import cats.syntax._
import lzwpack.data.BitBuffer

/**
  * Packs a stream of bits into a byte sequence so that a byte can contain multiple bit sequences.
  */
object Format {


  /**
    * Appends the given bit sequence to the given byte buffer (represented as an unsigned 32-bit integer).
    * The value is inserted at the least significant bit position in the buffer, and if the buffer fills the remainder
    * is returned in an overflow buffer (as the Segment's result).
    *
    * @return a {@see Segment} with an overflow buffer result
    */
  def appendToByte(buffer: BitBuffer)(code: Code, codeSize: Int): Segment[Byte, BitBuffer] = {
    if (codeSize == 0) return Segment(buffer.data.toByte).asResult(buffer)
    // Put as many bits as possible to the byte in buffer
    val nextBuffer = ((code << buffer.size) ^ buffer.data) & 0xFF
    val insertedBits = Math.min(8 - buffer.size, codeSize)
    val overflowSize = codeSize - insertedBits

    // Test if the the buffer is full
    if (overflowSize > 0) {
      // Put the remaining bits from the input into the overflow buffer
      val overflow = code >>> insertedBits
      // First chunk is full, so emit it; start to accumulate with second byte.
      Segment(nextBuffer.toByte).flatMapResult(_ => appendToByte(BitBuffer.empty)(overflow, overflowSize))
    } else {
      // First chunk is possibly not full, so don't emit it and try to accumulate more
      Segment.pure(BitBuffer(nextBuffer, buffer.size + insertedBits))
    }
  }

  /**
    * Returns a new {@see Pipe} that packs input tuples of code and its binary length in bits, across byte boundaries.
    */
  def pack[F[_]]: Pipe[F, (Code, Int), Byte] = {
    in => in.scanSegments(BitBuffer.empty) {
      case (buffer, segment) =>
        segment.flatMapAccumulate(buffer) {
          case (buffer, (code, codeSize)) => appendToByte(buffer)(code, codeSize)
        } mapResult (_._2)
    }
  }

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = {
    def go(stream: Stream[F, Byte], state: (Int, BitBuffer)): Pull[F, Code, Unit] = state match {
      case (counter, buf) =>
        val bytesToRead = ((counter.bitLength - buf.size) / 8) + 1

        stream.pull.unconsN(bytesToRead).flatMap {
          case Some((bytes, rest)) =>
            val (_, product) = bytes.fold(buf)((acc, b) => acc prepend BitBuffer(b)).force.run
            val (code, newBuffer) = product.read(counter.bitLength)
            System.err.println(show"[unpack]\t${counter.hex}: buffer ${product}\t\tread ${code.bin(counter.bitLength)}\t\temit ${code.hex}\t\tbuffer ${newBuffer}")
            Pull.output1(code) >> go(rest, (counter + 1, newBuffer))
          case None =>
            val values = buf.drain(counter.bitLength)
            for {
              value <- values
            } yield {
              System.err.println(show"[unpack]\tread EOF\t\temit ${value.hex}")
            }

            Pull.outputChunk(Chunk.array(values)) >> Pull.done
        }
    }
    in => go(in, (alphabet.size + 2, BitBuffer.empty)).stream
  }
}