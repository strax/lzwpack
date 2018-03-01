package lzwpack

import cats.tests.StrictCatsEquality
import org.scalatest._
import org.scalatest.prop.Checkers

abstract class UnitSpec extends FunSpec
  with Matchers
  with OptionValues
  with Inside
  with Inspectors
  with Checkers
  with DisciplineHelpers
  with StrictCatsEquality