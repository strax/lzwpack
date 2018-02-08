package lzwpack

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

  def size: Int
}

trait MakeDict { self =>
  def empty[A]: Dict[A]
  def fromAlphabet[A](alphabet: Alphabet[A]): Dict[A] = alphabet.foldLeft(empty[A])((z, a) => z.add(a))

  @deprecated
  def init[A]: Alphabet[A] => Dict[A] = fromAlphabet[A]
}

/**
  * A [[Dict]] optimized for compression.
  * Provides amortized O(1) add and lookup by key, but O(n) lookup by code.
  */
case class CompressionDict[K](map: Map[K, Code], currentCode: Int) extends Dict[K] {
  override def contains(key: K): Boolean = map contains key

  override def add(key: K): Dict[K] = CompressionDict(map + (key -> nextCode), nextCode)

  override def get(key: K): Code = map(key)

  override def find(code: Code): Option[K] = map find { case (_, other) => code == other } map (_._1)

  override def size: Code = map.size
}

object CompressionDict extends MakeDict {
  def empty[K]: Dict[K] = CompressionDict(Map.empty[K, Code], 0)
}

/**
  * A [[Dict]] optimized for decompression.
  * This is essentially the dual of [[CompressionDict]], so key lookup is O(n) while
  * code lookup and add are O(1) amortized.
  */
case class DecompressionDict[K](map: Map[Code, K], currentCode: Int) extends Dict[K] {
  override def contains(key: K): Boolean = getOption(key).nonEmpty

  override def add(key: K): Dict[K] = DecompressionDict(map + (nextCode -> key), nextCode)

  override def get(key: K): Code = getOption(key) getOrElse (throw new NoSuchElementException())

  override def find(code: Code): Option[K] = map get code

  override def size: Code = map.size

  def getOption(key: K): Option[Code] = map find { case (_, k) => k == key } map (_._1)
}

object DecompressionDict extends MakeDict {
  override def empty[K]: Dict[K] = DecompressionDict(Map.empty[Code, K], 0)
}