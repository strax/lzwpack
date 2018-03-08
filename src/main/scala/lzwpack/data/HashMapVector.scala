package lzwpack.data

import cats.{Eval, Now, Hash}
import cats.implicits._

/**
  * A [[HashMapVector]] is a hash map implementation on top of a bit-indexed trie.
  * As such, it provides the same functionality and performance characteristics as
  * [[SparseVector]], but instead of an integer key we can use any JVM object with
  * `hashCode` implemented as the key.
  */
class HashMapVector[K: Hash, @specialized V] private[data](private val vector: SparseVector[HashMapVector[K, V]#Bucket]) {
  // Use overflow lists ("buckets") to handle hashCode collisions
  type Bucket = ListVector[(K, V)]

  private def bucketForKey(key: K): Bucket = vector.get(key.hash).getOrElse(ListVector.empty)

  // Sets (k -> v) in the given bucket
  private def addOrReplaceInBucket(bucket: Bucket)(kv: (K, V)): Bucket = kv match {
    case (key, _) => bucket.filter(_._1 =!= key) + kv
  }

  /**
    * Returns a value for a given key.
    *
    * @throws scala.NoSuchElementException if there's no mapping from the given key to a value
    */
  def apply(key: K): V = get(key).getOrElse(throw new NoSuchElementException)

  /**
    * Returns whether this HashMapVector contains a value for a given key.
    */
  def contains(key: K): Boolean = get(key).nonEmpty

  /**
    * Returns an [[scala.Some]] with the value for the given key, or [[scala.None]] if there's no value.
    */
  def get(key: K): Option[V] = for {
    (_, value) <- bucketForKey(key).find(_._1 === key)
  } yield value

  /**
    * Returns a new HashMapVector with the given key-value mapping changed.
    */
  def updated(kv: (K, V)): HashMapVector[K, V] =
    new HashMapVector(vector.updated(kv._1.hash, addOrReplaceInBucket(bucketForKey(kv._1))(kv)))

  /**
    * Alias for [[updated]]
    */
  def +(kv: (K, V)): HashMapVector[K, V] = updated(kv)

  /**
    * Returns the number of key-value mappings in this HashMapVector.
    */
  lazy val size: Int = vector.fold(0) { case ((_, xs), acc) => xs.size + acc }

  /**
    * Returns true if this HashMapVector contains no elements, false otherwise.
    */
  def isEmpty: Boolean = vector.isEmpty

  /**
    * Folds the values of this HashMapVector with the given initial value and function.
    * The order for traversing the values is unspecified.
    */
  def fold[B](init: => B)(f: ((K, V), => B) => B): B =
    vector.fold(init) { case ((_, kvs), rest) => kvs.foldRight(Eval.later(rest))((h, t) => Now(f(h, t.value))).value }

  /**
    * Returns the first element that `f` returns true for. If no element matches `f` then [[scala.None]] is returned.
    * The order for traversing the values is unspecified.
    */
  def find(f: (K, V) => Boolean): Option[(K, V)] = fold(Option.empty[(K, V)]) {
    case ((k, v), acc) if f(k, v) => Some((k, v))
    case (_, acc) => acc
  }

  override def toString: String = s"HashMapVector($size elements)"
}

object HashMapVector {
  /**
    * Given a sequence of key-value tuples, constructs a new HashMapVector from them.
    */
  def apply[K: Hash, V](kvs: (K, V)*): HashMapVector[K, V] = {
    kvs.foldLeft(empty[K, V])((map, kv) => map + kv)
  }

  /**
    * Returns an empty HashMapVector.
    */
  def empty[K: Hash, V] = new HashMapVector[K, V](SparseVector.empty)
}