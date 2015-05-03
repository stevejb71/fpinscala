package fpinscala

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] = flatMap(ma)(a => map(mb)(f(a,_)))
  /* -------- Exercise 11.3 ------- */
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldLeft(unit(Nil:List[A]))((acc,b) => map2(b,acc)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)
}

object Monads {
  /* -------- Exercise 11.1 ------- */

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A) = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A) = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A) = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }
}