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
}
