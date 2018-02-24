package lzwpack.data

import ListVector._
import cats.{Eq, Functor, MonoidK, Show}
import cats.implicits._

class ListVector[A] private[data](private[data] val trie: SparseVector[A], val size: Int, offset: Int = 0) {
  def filter(f: A => Boolean): ListVector[A] = foldLeft(empty[A])((as, a) => if (f(a)) as + a else as)

  def find(f: A => Boolean): Option[A] = foldLeft(None : Option[A]) { (found, a) => found orElse (Some(a) filter f) }

  def apply(i: Int): A = get(i).getOrElse(throw new IndexOutOfBoundsException(s"$i > $size (offset $offset)"))

  def head: A = this(0)

  def tail: ListVector[A] = new ListVector[A](trie, size - 1, offset + 1)

  def append(a: A) = new ListVector(trie.updated(offset + size, a), size + 1)

  def get(i: Int): Option[A] = trie get (offset + i)

  def +(a: A): ListVector[A] = append(a)

  def isEmpty: Boolean = size == 0

  def foldLeft[B](init: B)(f: (B, A) => B): B = trie.foldLeft(init) { case (acc, (_, v)) => f(acc, v) }

  def intersperse(sep: A): ListVector[A] = foldLeft(empty[A]) { (acc, a) =>
    if (acc.isEmpty) acc + a else acc + sep + a
  }
}

object ListVector {
  def empty[A] = new ListVector(SparseVector.empty[A], 0)

  def apply[A](as: A*): ListVector[A] = as.foldLeft(empty[A])(_ + _)
}

trait ListVectorInstances {
  implicit object ListVectorFunctor extends Functor[ListVector] {
    override def map[A, B](fa: ListVector[A])(f: A => B): ListVector[B] =
      fa.foldLeft(empty[B])((acc, a) => acc + f(a))
  }

  implicit object ListVectorMonoidK extends MonoidK[ListVector] {
    override def empty[A]: ListVector[A] = ListVector.empty[A]

    override def combineK[A](x: ListVector[A], y: ListVector[A]): ListVector[A] =
      y.foldLeft(x)(_ + _)
  }

  implicit def ListVectorShow[A: Show]: Show[ListVector[A]] = { as =>
    s"ListVector(${as.map(_.show).intersperse(", ")})"
  }

  implicit def ListVectorEq[A: Eq]: Eq[ListVector[A]] = { (as, bs) =>
    @annotation.tailrec
    def rec(as: ListVector[A], bs: ListVector[A]): Boolean =
      as.head === bs.head && rec(as.tail, bs.tail)

    as.size === bs.size && (as.isEmpty && bs.isEmpty || rec(as, bs))
  }
}