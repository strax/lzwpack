package lzwpack

import org.scalacheck.Properties
import org.scalactic.Prettifier
import org.typelevel.discipline.Laws
import org.scalatest.TestRegistration
import org.scalatest.prop.Checkers

trait DisciplineHelpers extends Checkers { self: TestRegistration =>
  // Conversion from a Discipline RuleSet to a Scalacheck Properties
  implicit def ruleSetToProperties(rules: Laws#RuleSet): Properties = rules.all

  def properties(props: Properties): Unit = {
    for ((name, prop) <- props.properties) {
      registerTest(name) { check(name |: prop) }
    }
  }
}