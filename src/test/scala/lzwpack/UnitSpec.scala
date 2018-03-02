package lzwpack

import cats.Eq
import cats.instances.AllInstances
import cats.syntax.{AllSyntax, EqOps, EqSyntax}
import cats.tests.StrictCatsEquality
import org.scalatest._
import org.scalatest.prop.Checkers

abstract class UnitSpec extends FunSpec
  with Matchers
  with OptionValues
  with Inside
  with Checkers
  with DisciplineHelpers
  with AllSyntax
  with AllInstances
  with StrictCatsEquality
{
  // Disable Eq syntax since it collides with scalatest
  override def catsSyntaxEq[A: Eq](a: A): EqOps[A] = new EqOps[A](a)
}