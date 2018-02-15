package lzwpack.data

class HashMapVector[K, +V] private[data](private val vector: SparseVector[(K, V)]) { self =>
  private def hash(key: K): Int = key.hashCode

  def apply(key: K): V = get(key).getOrElse(throw new NoSuchElementException)

  def contains(key: K): Boolean = get(key).nonEmpty

  def get(key: K): Option[V] = vector.get(hash(key)).map(_._2)

  def updated[VV >: V](kv: (K, VV)): HashMapVector[K, VV] = kv match {
    case (k, v) => new HashMapVector(vector.updated[(K, VV)](hash(k), (k, v)))
  }

  def +[VV >: V](kv: (K, VV)): HashMapVector[K, VV] = updated(kv)

  def size: Int = vector.size

  def isEmpty: Boolean = vector.isEmpty

  def foldLeft[B](init: B)(f: (B, (K, V)) => B): B =
    vector.foldLeft(init) { case (acc, (_, kv)) => f(acc, kv) }

  def find(f: (K, V) => Boolean): Option[(K, V)] = foldLeft(Option.empty[(K, V)]) {
    case (None, (k, v)) if f(k, v) => Some((k, v))
    case (acc, _) => acc
  }
}

object HashMapVector {
  def apply[K, V](kvs: (K, V)*): HashMapVector[K, V] = {
    kvs.foldLeft(empty[K, V])((map, kv) => map + kv)
  }

  def empty[K, V] = new HashMapVector[K, V](SparseVector.empty)
}