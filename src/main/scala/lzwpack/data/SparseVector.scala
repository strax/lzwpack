package lzwpack.data

import lzwpack._

import scala.annotation.unchecked.uncheckedVariance
import SparseVector._
import cats.Eval

/**
  * A [[SparseVector]] is a bit-mapped trie with 32 branches at each node.
  * It supports O(log32(n)) insertion, updating and random access.
  */
sealed trait SparseVector[@specialized +A] {
  def apply(i: Int): A = get(i).getOrElse(throw new NoSuchElementException)

  def contains(i: Int): Boolean = get(i).nonEmpty

  def get(i: Int): Option[A]

  def updated[AA >: A](i: Int, a: AA): SparseVector[AA]

  def +[AA >: A](kv: (Int, AA)): SparseVector[AA] = kv match {
    case (k, v) => updated(k, v)
  }

  def fold[B](init: => B)(f: ((Int, A), => B) => B): B

  // Note: O(n) complexity because we don't really keep track of the bounds
  lazy val size: Int = fold(0)((_, n) => n + 1)

  def find(f: (Int, A) => Boolean): Option[(Int, A)] = fold(Option.empty[(Int, A)]) {
    case ((k, v), _) if f(k, v) => Some((k, v))
    case (_, acc) => acc
  }

  def isEmpty: Boolean
}

// Type parameter variance on Array is suppressed due to it being invariant; we can maintain the invariant ourselves
// by never mutating the array
private[data] case class Branch[+A](private val subforest: Array[SparseVector[A @uncheckedVariance]]) extends SparseVector[A] {
  def get(i: Int): Option[A] = {
    subforest(i & Bitmask).get(i >>> ChunkSize)
  }

  def updated[AA >: A](i: Int, a: AA): SparseVector[AA] = {
    // Make a new copy of the array to preserve persistence
    val copy = new Array[SparseVector[AA]](Radix)
    subforest.copyToArray(copy)
    copy.update(i & Bitmask, copy(i & Bitmask).updated(i >>> ChunkSize, a))

    Branch[AA](copy)
  }


  /**
    * Folds this trie in a depth-first manner.
    */
  override def fold[B](init: => B)(f: ((Int, A), => B) => B): B = {
    var acc = init
    for ((subtree, prefix) <- subforest.zipWithIndex if !subtree.isEmpty) {
      acc = subtree.fold(acc) { case ((suffix, v), acc) =>
        val key = (suffix << ChunkSize) ^ prefix
        f((key, v), acc)
      }
    }
    acc
  }

  override val isEmpty: Boolean = false
}

private[data] case class Leaf[+A](key: Int, value: A) extends SparseVector[A] {
  def get(i: Int) = if (key == i) Some(value) else None
  def updated[AA >: A](i: Int, a: AA): SparseVector[AA] = {
    if (key == i) {
      // Update the existing element
      Leaf(key, a)
    } else {
      // Convert this Leaf to a Branch with the new and current values as leaves
      EmptyBranch.updated(key, value).updated(i, a)
    }
  }


  override def fold[B](init: => B)(f: ((Int, A), => B) => B): B = f((key, value), init)

  override val isEmpty: Boolean = false
}

private[data] case object Empty extends SparseVector[Nothing] {
  def get(i: Int): Option[Nothing] = None
  def updated[A](i: Int, a: A): SparseVector[A] = Leaf(i, a)

  override val isEmpty: Boolean = true

  override def fold[B](init: => B)(f: ((Int, Nothing), => B) => B): B = init
}

object SparseVector {
  val Radix = 32
  private[data] val ChunkSize = Radix.bitsize - 1
  private[data] val Bitmask = Radix - 1 // Radix must be a power of 2, so (Radix - 1) masks values 0..Radix

  def apply[A](as: A*): SparseVector[A] = as.zipWithIndex.foldLeft(EmptyBranch: SparseVector[A]) {
    case (acc, (a, i)) => acc.updated(i, a)
  }

  def empty[A]: SparseVector[A] = Empty

  // we only need a single empty branch instance
  private[data] val EmptyBranch: SparseVector[Nothing] = Branch(Array.fill(Radix)(empty))
}