package fpinscala

import fpinscala.Monads._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

abstract class MonadLaws[A, B, C, M[_]](name: String, m: Monad[M], f: A => M[B], g: B => M[C])(implicit val arb: Arbitrary[M[A]]) extends Properties(name) {
  private implicit val imp = m

  property("associativity") = {
    forAll { (x: M[A]) => x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g)) }
  }
}

object OptionMonadLaws extends MonadLaws[String, Int, Double, Option](
  "Option monad",
  optionMonad,
  (s: String) => if (s.isEmpty) None else Some(s.length),
  (n: Int) => if (n > 3) None else Some(n.toDouble)
)

object ListMonadLaws extends MonadLaws[String, Int, Double, List](
  "List monad",
  listMonad,
  (s: String) => if (s.isEmpty) Nil else List(s.length, s.length+1),
  (n: Int) => if (n > 3) Nil else List(n.toDouble, n.toDouble+1.0, n.toDouble+2.0)
)

