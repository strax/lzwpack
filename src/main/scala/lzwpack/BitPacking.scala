package lzwpack

import fs2.{Pipe, Segment}

/**
  * Packs a stream of bits into a byte sequence so that a byte can contain multiple bit sequences.
  */
object BitPacking {
  /**
    * Returns the bit count (from the most significant bit) of a given unsigned 32-bit integer.
    */
  def bitLength(n: Int): Int = if(n == 0) 0 else Math.floor(Math.log(n) / Math.log(2)).toInt + 1

  /**
    * Appends the given bit sequence to the given byte buffer (represented as an unsigned 32-bit integer).
    * The value is inserted at the least significant bit position in the buffer, and if the buffer fills the remainder
    * is returned in an overflow buffer (as the Segment's result).
    *
    * @return a {@see Segment} with an overflow buffer result
    */
  def appendToByte(n: Int, buffer: Int): Segment[Byte, Int] = {
    if (n == 0) return Segment(buffer.toByte).asResult(buffer)
    // MSB first so new bytes are appended "from the right to the left"
    val bufferLength = bitLength(buffer)
    // Put as many bits as possible to the byte in buffer
    val nextBuffer = ((n << bufferLength) ^ buffer) & 0xFF

    // Test if the the buffer is full
    if (nextBuffer >= 0x80) {
      // Put the remaining bits from the input into the overflow buffer
      val overflowBuffer = (n >>> (8 - bufferLength))
      // First chunk is full, so emit it; start to accumulate with second byte.
      Segment(nextBuffer.toByte).flatMapResult(_ => appendToByte(overflowBuffer, 0))
    } else {
      // First chunk is possibly not full, so don't emit it and try to accumulate more
      Segment.pure(nextBuffer)
    }
  }

  def pack[F[_]]: Pipe[F, Int, Byte] = {
    in => in.scanSegments(0) {
      case (buffer, segment) =>
        segment.flatMapAccumulate(buffer)((z, n) => appendToByte(n, z)).mapResult(_._2)
    }
  }
}
