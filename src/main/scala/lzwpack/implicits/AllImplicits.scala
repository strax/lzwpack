package lzwpack.implicits

/**
  * Mixins all implicits into a single trait.
  */
trait AllImplicits extends ByteImplicits with LongImplicits with StringImplicits
