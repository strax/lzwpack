package lzwpack

import fs2.{Pipe, Segment}

/**
  * Packs a stream of bits into a byte sequence so that a byte can contain multiple bit sequences.
  */
object BitPacking {
  private[BitPacking] case class Buffer(data: Int, size: Int) {
    def available = 8 - size
  }
  private[BitPacking] object Buffer {
    def empty: Buffer = Buffer(0, 0)
  }

  /**
    * Appends the given bit sequence to the given byte buffer (represented as an unsigned 32-bit integer).
    * The value is inserted at the least significant bit position in the buffer, and if the buffer fills the remainder
    * is returned in an overflow buffer (as the Segment's result).
    *
    * @return a {@see Segment} with an overflow buffer result
    */
  def appendToByte(buffer: Buffer)(code: Code, codeSize: Int): Segment[Byte, Buffer] = {
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
      Segment(nextBuffer.toByte).flatMapResult(_ => appendToByte(Buffer.empty)(overflow, overflowSize))
    } else {
      // First chunk is possibly not full, so don't emit it and try to accumulate more
      Segment.pure(Buffer(nextBuffer, buffer.size + insertedBits))
    }
  }

  /**
    * Returns a new {@see Pipe} that packs input tuples of code and its binary length in bits, across byte boundaries.
    */
  def pack[F[_]]: Pipe[F, (Code, Int), Byte] = {
    in => in.scanSegments(Buffer.empty) {
      case (buffer, segment) =>
        segment.flatMapAccumulate(buffer) {
          case (buffer, (code, codeSize)) => appendToByte(buffer)(code, codeSize)
        } mapResult (_._2)
    }
  }
}
