package lzwpack

import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait DisciplineHelpers {
  implicit def ruleSetToProp(rules: Laws#RuleSet): Prop =
    rules.props.map { case (name, prop) => prop.label(name) }.reduce(_ && _)
}