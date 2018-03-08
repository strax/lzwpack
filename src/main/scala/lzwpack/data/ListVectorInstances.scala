package lzwpack.data

import cats.{Applicative, Eq, Eval, Monoid, Show, Traverse, ~>}
import lzwpack.data.ListVector.empty
import cats.implicits._
import cats.kernel.Hash

import scala.collection.{GenTraversable, TraversableLike}
import scala.collection.generic.{IsSeqLike, IsTraversableLike}

/**
  * Defines some generic type class instances for [[ListVector]].
  */
trait ListVectorInstances {
  implicit object ListVectorTraverse extends Traverse[ListVector] {
    override def traverse[F[_]: Applicative, A, B](fa: ListVector[A])(f: A => F[B]): F[ListVector[B]] =
      fa.foldLeft(empty[B].pure[F])((acc, a) => acc.map2(f(a))(_ + _))

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

  /**
    * ListVector forms a monoid for all A types. It's more useful to have a Monoid instance
    * available than a MonoidK given the functionality is the same for both instances.
    */
  implicit def ListVectorMonoid[A]: Monoid[ListVector[A]] = new Monoid[ListVector[A]] {
    override def empty: ListVector[A] = ListVector.empty[A]

    override def combine(as0: ListVector[A], as1: ListVector[A]): ListVector[A] =
      as0 concat as1
  }

  implicit object ListVectorApplicative extends Applicative[ListVector] {
    override def pure[A](x: A): ListVector[A] = ListVector(x)

    override def ap[A, B](ff: ListVector[A => B])(fa: ListVector[A]): ListVector[B] =
      ff foldMap (fa map _)
  }

  implicit def ListVectorShow[A: Show]: Show[ListVector[A]] = { as =>
    s"ListVector(${as.map(_.show).intercalate(", ")})"
  }

  implicit def ListVectorHash[A: Hash]: Hash[ListVector[A]] = new Hash[ListVector[A]] {
    override def hash(x: ListVector[A]): Int = x.hashCode

    override def eqv(x: ListVector[A], y: ListVector[A]): Boolean = ListVectorEq[A].eqv(x, y)
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

    // If the sizes are the same and the hash codes are the same then check individual elements
    as.size == bs.size && as.hashCode == bs.hashCode && iter(as, bs)
  }

  implicit def toSeq[A](as: ListVector[A]): Seq[A] = as.toSeq
}