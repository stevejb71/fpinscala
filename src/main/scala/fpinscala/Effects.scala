package fpinscala

sealed trait Free[F[_],+A] {
  def flatMap[B](f: A => Free[F,B]): Free[F,B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F,B] = flatMap(f andThen (Return(_)))
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](resume: () => A) extends Free[F,A]
case class FlatMap[F[_],A,B](sub: Free[F,A], k: A => Free[F,B]) extends Free[F,B]

object Free {
  // -------- Exercise 13.1 --------
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa map f
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
  }
}