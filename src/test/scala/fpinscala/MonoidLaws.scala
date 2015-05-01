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

object OtherFunctionsSpecification extends Properties("Other") {
  property("string to int foldMap under addition") = {
    forAll {(as: List[String]) => foldMap(as, intAddition)(_.length) == as.map(_.length).sum}
  }

  property("foldRightViaFoldMap is the same as foldRight") = {
    val f = (_:String) + (_:String)
    forAll {(as: List[String], z: String) => foldRightViaFoldMap(as, z)(f) == as.foldRight(z)(f)}
  }

  property("foldLeftViaFoldMap is the same as foldLeft") = {
    val f = (_:String) + (_:String)
    forAll {(as: List[String], z: String) => foldLeftViaFoldMap(as, z)(f) == as.foldLeft(z)(f)}
  }

  property("foldMapV is the same as foldMap") = {
    forAll {(as: Vector[String]) => foldMapV(as,intMultiplication)(_.length) == foldMap(as.toList,intMultiplication)(_.length)}
  }
}
