package fpinscala

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import Monoids._

abstract class MonoidLaws[A](m: Monoid[A])(implicit a: Arbitrary[A]) extends Properties("Monoid") {
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

object ArbitraryInstances {
  import Gen._

  private val genWCStub: Gen[WC] = alphaStr.map(Stub)
  private val genWCPart: Gen[WC] = for {
    l <- alphaStr
    w <- posNum[Int]
    r <- alphaStr
  } yield Part(l,w,r)

  val arbWC = Arbitrary(oneOf(genWCStub, genWCPart))
}

object IntAdditionMonoidLaws extends MonoidLaws(intAddition)
object IntMultiplicationMonoidLaws extends MonoidLaws(intMultiplication)
object BooleanOrMonoidLaws extends MonoidLaws(booleanOr)
object BooleanAndMonoidLaws extends MonoidLaws(booleanAnd)
object WCMonoidLaws extends MonoidLaws(wcMonoid)(ArbitraryInstances.arbWC)
object MapMergeMonoidLaws extends MonoidLaws(mapMergeMonoid[Int,String](stringMonoid))

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

  property("wc") = {
    forAll {(words: List[String]) => {
      val nonEmptyWords = words.map(w => (if(w.isEmpty) "X" else w).replace(' ','X'))
      val s = nonEmptyWords.mkString(" ")
      wc(s) == words.length
    }}
  }

  property("bag contains all the elements of its input IndexedSeq") = {
    forAll {(as: Vector[Int]) => as.forall(bag(as).contains)}
  }

  property("bag is no bigger than its input IndexedSeq") = {
    forAll {(as: Vector[Int]) => bag(as).size <= as.size}
  }

  property("bag of a Vector of sz duplicates of m has 1 element with value m") = {
    forAll {(m: String, n: Int) => {
      val sz = 1 + (n % 100).abs
      bag(List.fill(sz)(m).toIndexedSeq).toList == List(m -> sz)
    }}
  }
}

object ListFoldableSpecification extends Properties("ListFoldable") {
  property("foldMap on int addition is map + sum") = {
    forAll {(as:List[String]) => ListFoldable.foldMap(as)(_.length)(intAddition) == (as map (_.length)).sum}
  }

  property("concatenate on List of String is mkString") = {
    forAll {(as:List[String]) => ListFoldable.concatenate(as)(stringMonoid) == as.mkString}
  }
}

