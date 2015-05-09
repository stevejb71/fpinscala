package fpinscala

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(Nil:List[B]))((a, fbs) => map2(f(a),fbs)(_ :: _))

  /* ------- Exercise 12.1 -------- */

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(ma, replicateM(n - 1, ma))(_ :: _)
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa,fb)((a,b) => (a,b))

  /* ------- Exercise 12.2 (also above in definition of map, map2) -------- */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f,a) => f(a))

  /* ------- Exercise 12.3 -------- */
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = apply(apply(map(fa)(f.curried))(fb))(fc)
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] = apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)
}