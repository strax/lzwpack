package lzwpack.data

trait Indexed[K, +V] {
  def contains(key: K): Boolean
  def get(key: K): Option[V]
  def updated[VV >: V](key: K, value: VV): Indexed[K, VV]
  def +[VV >: V](kv: (K, VV)): Indexed[K, VV] = kv match {
    case (k, v) => updated(k, v)
  }
}
