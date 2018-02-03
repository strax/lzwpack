package lzwpack.data

import lzwpack._
import cats._
import cats.implicits._

case class BitBuffer(private[data] val data: Int, size: Int) {
  assert(size >= 0)

  /**
    * Reads n bits from input and returns a tuple of (read bits, remaining bits).
    */
  def read(bits: Int): (BitBuffer, Int) = {
    if (bits > size) throw new IndexOutOfBoundsException(s"Tried to read $bits bits from a buffer with $size bits")
    (drop(bits), take(bits).toInt)
  }

  def drop(n: Int): BitBuffer = {
    val tail = data >>> n
    BitBuffer(tail, Math.max(size - n, 0))
  }

  def take(n: Int): BitBuffer = {
    val mask = (1 << n) - 1
    val head = data & mask
    BitBuffer(head, Math.min(size, n))
  }

  def toInt: Int = data
  def toByte: Byte = data.toByte

  /**
    * Returns a new buffer that contains this buffer preceded by the other buffer.
    */
  def append(other: BitBuffer): BitBuffer = {
    val bb = other.data
    BitBuffer((bb << size) ^ data, other.size + size)
  }

  /**
    * Extracts as many values from this buffer as possible. Trailing zero-filled chunks are discarded.
    * @param chunkSize the size of each read value in bytes
    * @return an array of values
    */
  def drain(chunkSize: Int): (BitBuffer, Array[Int]) = {
    assert(chunkSize > 0)
    val chunkCount = size / chunkSize
    val values = new Array[Int](chunkCount)

    @annotation.tailrec
    def go(i: Int, buffer: BitBuffer): Unit = {
      if (i < chunkCount) {
        buffer.read(chunkSize) match {
          case (rest, value) =>
            values(i) = value
            if (rest.data > 0) go(i + 1, rest)
          case _ =>
        }
      }
    }

    go(0, this)
    (drop(chunkCount * chunkSize), values)
  }
}

trait BufferInstances {
  implicit object BufferMonoid extends Monoid[BitBuffer] {
    override def empty: BitBuffer = BitBuffer.empty
    override def combine(a: BitBuffer, b: BitBuffer): BitBuffer = a append b
  }

  implicit object BufferShow extends Show[BitBuffer] {
    override def show(buf: BitBuffer): String = show"${buf.data.bin(buf.size)} (n=${buf.size})"
  }
}

object BitBuffer {
  @inline def apply(b: Byte): BitBuffer = BitBuffer(b.unsigned, 8)
  @inline def apply(data: Int): BitBuffer = BitBuffer(data, data.bitLength)

  def tupled(t: (Int, Int)): BitBuffer = BitBuffer(t._1, t._2)

  def empty: BitBuffer = BitBuffer(0, 0)
}