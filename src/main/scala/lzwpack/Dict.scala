package lzwpack

/**
  * A dictionary contains unique codes for given chunks in the input
  *
  * @todo Change Map to custom trie
  */
case class Dict[A](entries: Map[A, Code], headIndex: Int) {
  /**
    * Returns a boolean determining if this dictionary contains a given chunk
    */
  def contains(a: A): Boolean = entries.contains(a)

  /**
    * Adds a new chunk to the dictionary
    * @return a new dictionary with the inserted element
    */
  def add(a: A): Dict[A] = {
    val newIndex = headIndex + 1
    Dict(entries + (a -> newIndex), newIndex)
  }

  def nextIndex: Int = headIndex + 1

  def codeSize: Int = headIndex.bitLength

  /**
    * Returns the code associated with the given chunk.
    * @throws java.util.NoSuchElementException if this dictionary does not contain the given chunk
    */
  def get(a: A): Code = entries(a)

  def reverseGet(code: Code): Option[A] = entries.find {
    case (_, other) => code == other
  }.map(_._1)

  private[Dict] def inc: Dict[A] = Dict(this.entries, this.headIndex + 1)
}

/**
  * Companion object for {@see Dict}
  */
object Dict {
  def empty[A] = Dict(Map.empty[A, Code], 0)

  /**
    * Initializes a new dictionary with the given alphabet.
    * In LZW, the dictionary always contains codes for the whole alphabet before compression / decompression.
    */
  def init[A](alphabet: Alphabet[A]): Dict[A] =
    alphabet.foldLeft(empty[A])((z, a) => z.add(a))
}

