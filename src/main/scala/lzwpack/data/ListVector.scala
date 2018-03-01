package lzwpack.data

import ListVector._
import cats._
import cats.implicits._

// Note: we could use kittens to derive some of these, but let's do it manually for the sake of learning
trait ListVectorInstances {
  trait ListVectorFoldable extends Foldable[ListVector] {
    override def foldLeft[A, B](fa: ListVector[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    /**
      * Lazy right fold. Using [[Eval]] is enforced by the typeclass, but it allows us to short-circuit some
      * folds if necessary.
      */
    override def foldRight[A, B](fa: ListVector[A], init: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case h :: t => f(h, Eval.defer(foldRight(t, init)(f)))
      case _ => init
    }
  }

  implicit object ListVectorTraverse extends Traverse[ListVector] with ListVectorFoldable {
    override def traverse[F[_]: Applicative, A, B](fa: ListVector[A])(f: A => F[B]): F[ListVector[B]] =
      fa.foldLeft(empty[B].pure[F])((acc, a) => acc.map2(f(a))(_ + _))
  }

  implicit object ListVectorMonoidK extends MonoidK[ListVector] {
    override def empty[A]: ListVector[A] = ListVector.empty[A]

    override def combineK[A](x: ListVector[A], y: ListVector[A]): ListVector[A] =
      y.foldLeft(x)(_ + _)
  }

  implicit def ListVectorShow[A: Show]: Show[ListVector[A]] = { as =>
    s"ListVector(${as.map(_.show).intercalate(", ")})"
  }

  /**
    * Strict equality for two list vectors. The implementation is O(n) as we
    * go through each of the list elements and compare them.
    *
    * @note Could be implemented with foldRight
    */
  implicit def ListVectorEq[A: Eq]: Eq[ListVector[A]] = { (as, bs) =>
    @annotation.tailrec
    def iter(as: ListVector[A], bs: ListVector[A]): Boolean =
      if (as.isEmpty && bs.isEmpty) true else as.head === bs.head && iter(as.tail, bs.tail)

    as.size == bs.size && iter(as, bs)
  }
}

/**
  * A [[ListVector]] is a list-like data structure in the sense that it provides
  * efficient random access and append; prepend is not yet supported.
  * Compared to a [[SparseVector]] this data structure is _bounded_ meaning that given a list with size |L| > 0
  * there is guaranteed to be an element in each k ∈ [0..|L| - 1]. Updates to indices outside of the index range are prohibited.
  */
class ListVector[A] private[data](private[data] val trie: SparseVector[A], val size: Int, offset: Int = 0) {
  /**
    * Returns a new [[ListVector]] for which f(a) == true for all elements.
    * Note that indices are not preserved; for instance, if index 3 was filtered out and 4 was not,
    * then index 4 would become index 3. This is to preserve list-like semantics.
    */
  def filter(f: A => Boolean): ListVector[A] = foldLeft(empty[A])((as, a) => if (f(a)) as + a else as)

  /**
    * Returns the first element `a` in this [[ListVector]] for which `f(a)` returns true, wrapped in an [[Option]].
    * If no element matches `f`, then a [[None]] is returned.
    */
  def find(f: A => Boolean): Option[A] = foldLeft(None : Option[A]) { (found, a) => found orElse (Some(a) filter f) }

  /**
    * Unsafe version of [[get]].
    * @throws IndexOutOfBoundsException if this [[ListVector]] does not contain the given index
    */
  def apply(i: Int): A = get(i).getOrElse(throw new IndexOutOfBoundsException(s"$i > $size (offset $offset)"))

  /**
    * Returns the first element of this [[ListVector]].
    * @throws IndexOutOfBoundsException if this [[ListVector]] is empty
    */
  def head: A = this(0)

  def headOption: Option[A] = get(0)

  /**
    * Returns a [[ListVector]] with all the elements from this vector except the first one (see [[head]]).
    * The first index is dropped so that
    * {{{
    *   ListVector(1,2,3).tail.get(0) == Some(2)
    * }}}
    */
  def tail: ListVector[A] = new ListVector[A](trie, size - 1, offset + 1)

  /**
    * Appends the given value to the end of this [[ListVector]].
    * Complexity: O(log n)
    */
  def append(a: A) = new ListVector(trie.updated(offset + size, a), size + 1)

  /**
    * If there exists an element with the given index, returns an [[Option]] containing the element or
    * an empty option otherwise.
    * Complexity: O(log n)
    */
  def get(i: Int): Option[A] = trie get (offset + i)

  /**
    * Returns true if this [[ListVector]] contains an element at index `i`, `false` otherwise.
    */
  def contains(i: Int): Boolean = get(i).isDefined

  /**
    * Alias for [[append]].
    */
  def +(a: A): ListVector[A] = append(a)

  /**
    * Returns `true` if this [[ListVector]] is empty (i.e. contains no elements), `false` otherwise.
    */
  def isEmpty: Boolean = size == 0

  /**
    * Left folds the elements of this [[ListVector]].
    */
  def foldLeft[B](init: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def iter(as: ListVector[A], acc: B): B = as match {
      case h :: t => iter(t, f(acc, h))
      case _ => acc
    }
    iter(this, init)
  }

  /**
    * Returns an [[Iterator]] for this [[ListVector]].
    * @note we do not implement the Iterable trait in order to make our own data structure methods
    */
  def iterator: Iterator[A] = new Iterator[A] {
    private var i = offset

    override def hasNext: Boolean = ListVector.this.contains(i)

    override def next(): A = {
      val a = ListVector.this(i)
      i += 1
      a
    }
  }

  def map[B](f: A => B): ListVector[B] =
    foldLeft(empty[B])((acc, a) => acc + f(a))

  override def toString: String = s"ListVector(${this.map(_.toString).intercalate(", ")})"
}

object ListVector {
  def empty[A] = new ListVector(SparseVector.empty[A], 0)

  def apply[A](as: A*): ListVector[A] = as.foldLeft(empty[A])(_ + _)
}

object :: {
  def unapply[A](as: ListVector[A]): Option[(A, ListVector[A])] = as.headOption.map((_, as.tail))
}