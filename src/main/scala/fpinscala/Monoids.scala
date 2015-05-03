package fpinscala

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }
  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /* ------- Exercise 10.1 ------- */

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }
  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }
  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }
  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  /* ------- Exercise 10.2 ------- */

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  /* ------- Exercise 10.3 ------- */

  type Endo[A] = A => A

  def endoMonoid[A] = new Monoid[Endo[A]] {
    override def op(a1: Endo[A], a2: Endo[A]): Endo[A] = a2 andThen a1
    override def zero: Endo[A] = identity
  }

  /* ------- Exercise 10.5 ------- */

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /* ------- Exercise 10.6 ------- */

  def foldRightViaFoldMap[A,B](as: List[A], b: B)(f: (A,B) => B): B = {
    foldMap(as, endoMonoid[B])(a => b => f(a,b))(b)
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2,a1)
    override def zero: A = m.zero
  }

  def foldLeftViaFoldMap[A,B](as: List[A], b: B)(f: (B,A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b,a))(b)
  }

  /* ------- Exercise 10.7 ------- */

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) {
      m.zero
    } else if(v.length == 1) {
      f(v.head)
    } else {
      val (l,r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l,m)(f), foldMapV(r,m)(f))
    }
  }

  /* -------- Exercise 10.10 -------- */

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def count(wc: WC): Int = {
    def oneIfNonEmpty(s: String) = if(s.isEmpty) 0 else 1
    wc match {
      case Stub(_) => 1
      case Part(l,w,r) => oneIfNonEmpty(l) + w + oneIfNonEmpty(r)
    }
  }

  val wcMonoid  = new Monoid[WC] {
    override def op(wc1: WC, wc2: WC) = (wc1,wc2) match {
      case (Stub(c1),Stub(c2)) => Stub(c1 + c2)
      case (Stub(s),Part(l,w,r)) => Part(s+l,w,r)
      case (Part(l,w,r),Stub(s)) => Part(l,w,r+s)
      case (Part(l1,w1,r1),Part(l2,w2,r2)) => Part(l1,w1+w2+1,r2)
    }
    override def zero = Stub("")
  }

  /* -------- Exercise 10.11 -------- */

  def wc(s: String): Int = {
    def toWC(c: Char) = c match {
      case ' ' => Part("",0,"")
      case _ => Stub(c.toString)
    }
    if(s.isEmpty) 0 else count(foldMapV(s.trim, wcMonoid)(toWC))
  }

  /* -------- Exercise 10.12 -------- */

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
    def toList[A](as: F[A]): List[A]
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def toList[A](as: List[A]): List[A] = as
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))
  }


}
