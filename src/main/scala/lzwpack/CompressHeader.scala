package lzwpack

import cats.data.Chain
import fs2.{Chunk, Stream}
import cats.implicits._
import cats.syntax._

/**
  * Provides compress(1) compatible file header coding and decoding.
  */
object CompressHeader {
  val Header: List[Byte] = List(0x1f.toByte, 0x9d.toByte)
  val CodeSizeMarker = 0x90.toByte

  def decode[F[_]](stream: Stream[F, Byte]): Stream[F, Byte] = stream.take(2).chunkN(2).flatMap { chunk =>
    if (chunk.toList == Header)
      stream.drop(3)
    else
      stream
  }

  def encode[F[_]](stream: Stream[F, Byte]): Stream[F, Byte] = {
    stream.cons(Chunk.seq(Header :+ CodeSizeMarker))
  }
}
