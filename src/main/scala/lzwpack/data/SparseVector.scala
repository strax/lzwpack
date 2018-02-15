package lzwpack.data

import lzwpack._

import scala.annotation.unchecked.uncheckedVariance
import SparseVector._

/**
  * A [[SparseVector]] is a bit-mapped trie with 32 branches at each node.
  * It supports O(log32(n)) insertion, updating and random access.
  */
sealed trait SparseVector[+A] {
  def contains(i: Int): Boolean = get(i).nonEmpty

  def get(i: Int): Option[A]

  def updated[AA >: A](i: Int, a: AA): SparseVector[AA]

  def +[AA >: A](kv: (Int, AA)): SparseVector[AA] = kv match {
    case (k, v) => updated(k, v)
  }

  def foldLeft[B](init: B)(f: (B, SparseVector[A]) => B): B

  def size: Int = foldLeft(0) { (acc, node) =>
    node match {
      case Leaf(_, _) => acc + 1
      case _ => acc
    }
  }

  def find(f: (Int, A) => Boolean): Option[(Int, A)] = ???

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


  override def foldLeft[B](init: B)(f: (B, SparseVector[A]) => B): B = {
    var acc = f(init, this)
    for (subtree <- subforest if !subtree.isEmpty) {
      acc = subtree.foldLeft(acc)(f)
    }
    acc
  }

  override def isEmpty: Boolean = false
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


  override def foldLeft[B](init: B)(f: (B, SparseVector[A]) => B): B = f(init, this)

  override def isEmpty: Boolean = false
}

private[data] case object Empty extends SparseVector[Nothing] {
  def get(i: Int): Option[Nothing] = None
  def updated[A](i: Int, a: A): SparseVector[A] = Leaf(i, a)

  override def isEmpty: Boolean = true

  override def foldLeft[B](init: B)(f: (B, SparseVector[Nothing]) => B) = f(init, this)
}

object SparseVector {
  val Radix = 32
  private[data] val ChunkSize = Radix.bitsize
  private[data] val Bitmask = Radix - 1 // Radix must be a power of 2, so (Radix - 1) masks values 0..Radix

  def apply[A](as: A*): SparseVector[A] = as.zipWithIndex.foldLeft(EmptyBranch: SparseVector[A]) {
    case (acc, (a, i)) => acc.updated(i, a)
  }

  def empty[A]: SparseVector[A] = Empty

  // we only need a single empty branch instance
  private[data] val EmptyBranch: SparseVector[Nothing] = Branch(Array.fill(Radix)(empty))
}