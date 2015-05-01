package fpinscala

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import Monoids._

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

object IntAdditionMonoidLaws extends MonoidLaws(intAddition)
object IntMultiplicationMonoidLaws extends MonoidLaws(intMultiplication)
object BooleanOrMonoidLaws extends MonoidLaws(booleanOr)
object BooleanAndMonoidLaws extends MonoidLaws(booleanAnd)

object FoldMapSpecification extends Properties("FoldMap") {
  property("string to int foldMap under addition") = {
    forAll {(as: List[String]) => foldMap(as, intAddition)(_.length) == as.map(_.length).sum}
  }

  property("foldRightViaFoldMap is the same as foldRight") = {
    val f = (a: String, b: String) => s"test${a}and${b}"
    forAll {(as: List[String], z: String) => foldRightViaFoldMap(as, z)(f) == as.foldRight(z)(f)}
  }
}
