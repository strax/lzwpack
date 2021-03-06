package lzwpack.data

import ListVector._
import cats._
import cats.implicits._


/**
  * A [[ListVector]] is a list-like data structure in the sense that it provides
  * efficient random access and append; prepend is not yet supported.
  * Compared to a [[SparseVector]] this data structure is _bounded_ meaning that given a list with size |L| > 0
  * there is guaranteed to be an element in each k ∈ [0..|L| - 1]. Updates to indices outside of the index range are prohibited.
  */
class ListVector[@specialized A] private[data](private[data] val trie: SparseVector[A], val size: Int, offset: Int = 0) {
  /**
    * Returns a new [[ListVector]] for which f(a) == true for all elements.
    * Note that indices are not preserved; for instance, if index 3 was filtered out and 4 was not,
    * then index 4 would become index 3. This is to preserve list-like semantics.
    */
  def filter(f: A => Boolean): ListVector[A] = foldLeft(empty[A])((as, a) => if (f(a)) as + a else as)

  /**
    * Concatenates the argument to this [[ListVector]].
    */
  def concat(as1: ListVector[A]): ListVector[A] = as1.foldLeft(this)(_ + _)

  /**
    * Returns the first element `a` in this [[ListVector]] for which `f(a)` returns true, wrapped in an [[scala.Option]].
    * If no element matches `f`, then a [[scala.None]] is returned.
    */
  def find(f: A => Boolean): Option[A] = foldRight(none[A]) { (a, rest) => if (f(a)) a.some else rest }

  /**
    * Unsafe version of [[get]].
    * @throws scala.IndexOutOfBoundsException if this [[ListVector]] does not contain the given index
    */
  def apply(i: Int): A = get(i).getOrElse(throw new IndexOutOfBoundsException(s"$i > $size (offset $offset)"))

  /**
    * Returns the first element of this [[ListVector]].
    * @throws scala.IndexOutOfBoundsException if this [[ListVector]] is empty
    */
  lazy val head: A = this(0)

  /**
    * Like [[head]], but returns an [[scala.Option]] determining the success of the operation instead of throwing
    * an exception.
    */
  lazy val headOption: Option[A] = get(0)

  /**
    * Returns a [[ListVector]] with all the elements from this vector except the first one (see [[head]]).
    * The first index is dropped so that
    * {{{
    *   ListVector(1,2,3).tail.get(0) == Some(2)
    * }}}
    */
  lazy val tail: ListVector[A] = drop(1)

  /**
    * Returns a new [[ListVector]] without the first `n` items.
    */
  def drop(n: Int): ListVector[A] = {
    if (n > size) empty[A] else new ListVector(trie, size - n, offset + n)
  }

  /**
    * Returns a new [[ListVector]] with only the first `n` items present.
    */
  def take(n: Int): ListVector[A] = new ListVector[A](trie, Math.min(n, size), offset)

  /**
    * Appends the given value to the end of this [[ListVector]].
    * Complexity: O(log n)
    */
  def append(a: A) = new ListVector(trie.updated(offset + size, a), size + 1)

  /**
    * If there exists an element with the given index, returns an [[scala.Option]] containing the element or
    * an empty option otherwise.
    * Complexity: O(log n)
    */
  def get(i: Int): Option[A] = if (i < size) trie get (offset + i) else none[A]

  /**
    * Returns true if this [[ListVector]] contains an element at index `i`, `false` otherwise.
    */
  def contains(i: Int): Boolean = get(i).isDefined

  /**
    * Alias for [[append]].
    */
  def +(a: A): ListVector[A] = append(a)

  /**
    * Alias for [[append]].
    */
  def :+(a: A): ListVector[A] = append(a)

  /**
    * Returns `true` if this [[ListVector]] is empty (i.e. contains no elements), `false` otherwise.
    */
  def isEmpty: Boolean = size == 0

  // Uses random access for speed (?)
  @annotation.tailrec
  private def foldl[B](start: Int, end: Int, init: B, f: (B, A) => B): B = {
    if (start == end) init
    else foldl(start + 1, end, f(init, this(start)), f)
  }

  private def foldr[B](start: Int, end: Int, init: => B, f: (A, => B) => B): B = {
    if (start == end) init
    else f(this(start), foldr(start + 1, end, init, f))
  }

  /**
    * Left folds the elements of this [[ListVector]].
    */
  def foldLeft[B](init: B)(f: (B, A) => B): B = foldl(0, size, init, f)

  /**
    * Right folds the elements of this ListVector.
    */
  def foldRight[B](init: => B)(f: (A, => B) => B): B = foldr(0, size, init, f)

  /**
    * Returns an [[scala.Iterator]] for this [[ListVector]].
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

  /**
    * Returns a new [[ListVector]] with each element applied to `f`.
    */
  def map[B](f: A => B): ListVector[B] =
    foldLeft(empty[B])((acc, a) => acc + f(a))

  override lazy val toString: String = s"ListVector(${this.map(_.toString).intercalate(", ")})"

  /**
    * Hash code calculated as in [[java.util.List]].
    */
  override lazy val hashCode: Int = foldLeft(1)((acc, e) => 31 * acc + e.hashCode())

  /**
    * Converts this ListVector to a [[scala.Seq]].
    */
  lazy val toSeq: Seq[A] = iterator.toSeq
}

object ListVector {
  def empty[A] = new ListVector(SparseVector.empty[A], 0)

  def apply[A](as: A*): ListVector[A] = seq(as)

  def seq[A](as: Seq[A]): ListVector[A] = as.foldLeft(empty[A])(_ + _)

  def unfold[A, S](init: S)(f: S => Option[(A, S)]): ListVector[A] = {
    @annotation.tailrec
    def iter(acc: ListVector[A], s1: S): ListVector[A] = f(s1) match {
      case None => acc
      case Some((a, s2)) => iter(acc + a, s2)
    }
    iter(empty[A], init)
  }

  def range[A: Integral](from: A, to: A)(implicit integral: Integral[A]): ListVector[A] = {
    import integral._
    // We need to append the upper bound explicitly in order to avoid numeric overflows when comparing values.
    unfold(from)(_.some.filter(_ < to).map(n => (n, n + one))) + to
  }
}

object :: {
  def unapply[A](as: ListVector[A]): Option[(A, ListVector[A])] = as.headOption.map((_, as.tail))
}