package fpinscala

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

abstract class MonoidLaws[A: Arbitrary](m: Monoid[A]) extends Properties("Monoid") {
  property("zero on the left") = {
    forAll {(a: A) => m.op(m.zero, a) == a}
  }

  property("zero on the right") = {
    forAll {(a: A) => m.op(a, m.zero) == a}
  }

  property("associativity") = {
    forAll {(a1: A, a2: A, a3: A) => m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)}
  }
}

object IntAdditionMonoidLaws extends MonoidLaws(Monoids.intAddition)
object IntMultiplicationMonoidLaws extends MonoidLaws(Monoids.intMultiplication)
object BooleanOrMonoidLaws extends MonoidLaws(Monoids.booleanOr)
object BooleanAndMonoidLaws extends MonoidLaws(Monoids.booleanAnd)
