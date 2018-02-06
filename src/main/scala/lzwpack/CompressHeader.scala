package lzwpack

import cats.Applicative
import fs2.{Pull, Segment, Stream}

/**
  * Provides compress(1) compatible file header coding and decoding.
  */
object CompressHeader {
  val Header: List[Byte] = List(0x1f.toByte, 0x9d.toByte)
  val CodeSizeMarker = 0x90.toByte

  def decode[F[_]](stream: Stream[F, Byte]): Stream[F, Byte] = stream.take(2).segmentN(2).flatMap { segment =>
    if (segment.force.toList == Header)
      stream.drop(3)
    else
      stream
  }

  def encode[F[_]](stream: Stream[F, Byte]): Stream[F, Byte] = stream.cons(Segment.seq(Header) ++ Segment(CodeSizeMarker))
}
