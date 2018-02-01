package lzwpack.data

import lzwpack._
import cats._
import cats.implicits._

case class Buffer private[data](data: Int, size: Int) {
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

trait BufferInstances {
  implicit object BufferMonoid extends Monoid[Buffer] {
    override def empty: Buffer = Buffer.empty

    override def combine(a: Buffer, b: Buffer): Buffer = b prepend a
  }

  implicit object BufferShow extends Show[Buffer] {
    override def show(buf: Buffer): String = show"${buf.data.bin(buf.size)} (n=${buf.size})"
  }
}

object Buffer {
  def apply(b: Byte): Buffer = Buffer(b.unsigned, 8)
  def apply(data: Int): Buffer = Buffer(data, data.bitLength)

  def empty: Buffer = Buffer(0, 0)
}