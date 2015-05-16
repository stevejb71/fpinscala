package fpinscala

object Util {
  implicit class TypeSafeEquals[A](val a1: A) extends AnyVal {
    def ===(a2: A) = a1 == a2
  }
}

import Util._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ApplicativeLaws {
  def functorIdentity[M[_], A, B](F: Applicative[M], v: M[A]): Boolean = F.map(v)(identity) === v
  def functorComposition[M[_], A, B, C](F: Applicative[M], v: M[A], f: B => C, g: A => B): Boolean = F.map(F.map(v)(g))(f) === F.map(v)(f compose g)

  def leftIdentity[M[_], A](F: Applicative[M], fa: M[A]): Boolean = F.map2(F.unit(()), fa)((_,a) => a) === fa
  def rightIdentity[M[_], A](F: Applicative[M], fa: M[A]): Boolean = F.map2(fa, F.unit(()))((a,_) => a) === fa

  def associativity[M[_],A,B,C](F: Applicative[M], fa: M[A], fb: M[B], fc: M[C]): Boolean = {
    def assoc(p: (A,(B,C))): ((A,B),C) = p match {
      case (a,(b,c)) => ((a,b),c)
    }
    F.product(F.product(fa,fb),fc) === F.map(F.product(fa,F.product(fb,fc)))(assoc)
  }
}

object TraversableProperties extends Properties("Traversable") {
  import Traverse.listTraverse

  property("list reverse") = forAll{(xs: List[Int]) => listTraverse.reverse(listTraverse.reverse(xs)) === xs}
}
