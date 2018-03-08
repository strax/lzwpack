package lzwpack

import cats.Eq
import cats.implicits._
import lzwpack.data.{HashMapVector, SparseVector}

/**
  * A dict is a set of keys that have an associated monotonic unique code.
  *
  * @tparam K the type of keys in the dict
  */
trait Dict[K] {
  /**
    * Returns true if this Dict contains the given key.
    */
  def contains(key: K): Boolean

  /**
    * Returns a new Dict that contains the given key mapped to the next code.
    */
  def add(key: K): Dict[K]

  /**
    * Returns the code that was previously added to this Dict.
    */
  def currentCode: Int

  /**
    * Returns the next code to assign to a value.
    */
  def nextCode: Int = currentCode + 1

  /**
    * Returns the associated code for the given value.
    * @throws NoSuchElementException if the key does not exist in this Dict
    */
  def get(key: K): Code

  /**
    * Returns the key with the given code or [[None]] if the code does not exist in this Dict.
    */
  def find(code: Code): Option[K]
}

/**
  * The [[MakeDict]] trait provides generic dictionary constructors; given a [[MakeDict[T]] we can construct
  * an empty T[A] or a T[A] that contains the given alphabet.
  */
trait MakeDict[T[_]] {
  def empty[A: Eq]: Dict[A]
  def fromAlphabet[A: Eq](alphabet: Alphabet[A]): Dict[A] = alphabet.foldLeft(empty[A])((z, a) => z.add(a))
}

/**
  * A [[Dict]] optimized for compression.
  * Provides amortized O(1) add and lookup by key, but O(n) lookup by code.
  */
case class CompressionDict[K](map: HashMapVector[K, Code], currentCode: Int) extends Dict[K] {
  override def contains(key: K): Boolean = map contains key

  override def add(key: K): Dict[K] = CompressionDict(map + (key -> nextCode), nextCode)

  override def get(key: K): Code = map(key)

  override def find(code: Code): Option[K] = map find { case (_, other) => code === other } map (_._1)
}

object CompressionDict extends MakeDict[CompressionDict] {
  def empty[K: Eq]: Dict[K] = CompressionDict(HashMapVector.empty[K, Code], 0)
}

/**
  * A [[Dict]] optimized for decompression.
  * This is essentially the dual of [[CompressionDict]], so key lookup is O(n) while
  * code lookup and add are O(1) amortized.
  */
case class DecompressionDict[K: Eq](map: SparseVector[K], currentCode: Int) extends Dict[K] {
  override def contains(key: K): Boolean = getOption(key).nonEmpty

  override def add(key: K): Dict[K] = DecompressionDict(map + (nextCode -> key), nextCode)

  override def get(key: K): Code = getOption(key) getOrElse (throw new NoSuchElementException())

  override def find(code: Code): Option[K] = map get code

  def getOption(key: K): Option[Code] = map find { case (_, k) => k === key } map (_._1)
}

object DecompressionDict extends MakeDict[DecompressionDict] {
  override def empty[K: Eq]: Dict[K] = DecompressionDict(SparseVector.empty[K], 0)
}

// Provide implicit MakeDict instances to support generic Dict creation
// The MakeDict instances are also companion objects to provide nicer syntax when calling concrete instances
trait DictInstances {
  implicit val makeCompressionDict: MakeDict[CompressionDict] = CompressionDict
  implicit val makeDecompressionDict: MakeDict[DecompressionDict] = DecompressionDict
}