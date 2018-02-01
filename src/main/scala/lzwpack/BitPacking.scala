package lzwpack

import fs2.{Chunk, Pipe, Pull, Segment, Stream}
import cats._
import cats.data._
import cats.implicits._
import cats.syntax._

/**
  * Packs a stream of bits into a byte sequence so that a byte can contain multiple bit sequences.
  */
object BitPacking {
  case class Buffer(data: Int, size: Int) {
    def available = 8 - size

    /**
      * Reads n bits from input and returns a tuple of (read bits, remaining bits).
      */
    def read(bits: Int): (Int, Buffer) = {
      if (bits > size) throw new IndexOutOfBoundsException(s"Tried to read $bits bits from a buffer with $size bits")
      val mask = (1 << bits) - 1
      val read = data & mask
      val rest = data >>> bits
      (read, Buffer(rest, size - bits))
    }

    /**
      * Returns a new buffer that contains this buffer preceded by the other buffer.
      */
    def prepend(other: Buffer): Buffer = {
      val bb = other.data
      Buffer((bb << size) ^ data, other.size + size)
    }

    /**
      * Extracts as many values from this buffer as possible. Trailing zero-filled chunks are discarded.
      * @param chunkSize the size of each read value in bytes
      * @return an array of values
      */
    def drain(chunkSize: Int): Array[Int] = {
      assert(chunkSize > 0)
      val chunkCount = size / chunkSize
      val values = new Array[Int](chunkCount)

      @annotation.tailrec
      def go(i: Int, buffer: Buffer): Unit = {
        if (i < chunkCount) {
          buffer.read(chunkSize) match {
            case (value, rest) =>
              values(i) = value
              if (rest.data > 0) go(i + 1, rest)
            case _ =>
          }
        }
      }

      go(0, this)
      values
    }
  }

  implicit object BufferMonoid extends Monoid[Buffer] {
    override def empty: Buffer = Buffer.empty

    override def combine(a: Buffer, b: Buffer): Buffer = b prepend a
  }

  implicit object BufferShow extends Show[Buffer] {
    override def show(buf: Buffer): String = show"${buf.data.bin(buf.size)} (n=${buf.size})"
  }

  def buffer(b: Byte): Buffer = Buffer(b.unsigned, 8)
  def buffer(data: Int): Buffer = Buffer(data, data.bitLength)

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

  def unpack[F[_]](implicit alphabet: Alphabet[Byte]): Pipe[F, Byte, Code] = {
    def go(stream: Stream[F, Byte], state: (Int, Buffer)): Pull[F, Code, Unit] = state match {
      case (counter, buf) =>
        val bytesToRead = ((counter.bitLength - buf.size) / 8) + 1

        stream.pull.unconsN(bytesToRead).flatMap {
          case Some((bytes, rest)) =>
            val (_, product) = bytes.fold(buf)((acc, b) => acc prepend buffer(b)).force.run
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
    in => go(in, (alphabet.size + 2, Buffer.empty)).stream
  }
}