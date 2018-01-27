import cats.Applicative

import scala.collection.immutable.List

package object lzwpack extends Implicits {
  /**
    * A {@see Code} represents the outputted code for a given input subsequence.
    */
  type Code = Int

  /**
    * An alphabet for a given input S is the set of possible members an input sequence can be composed from.
    *
    * @todo Implement Set
    */
  type Alphabet[A] = List[A]

  /**
    * Defines an implicit conversion to include Alphabet's methods by default.
    */
  implicit def alphabetOps[A](a: Alphabet[A]): Alphabet.Ops[A] = Alphabet.Ops(a)
}
