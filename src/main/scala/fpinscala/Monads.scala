package fpinscala

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
}


trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  /* -------- Exercise 11.8 ------- */
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_:Unit) => ma, f)(())
  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(f(a, _)))
  /* -------- Exercise 11.3 ------- */
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldLeft(unit(Nil: List[A]))((acc, b) => map2(b, acc)(_ :: _))
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)
  /* -------- Exercise 11.4 ------- */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(ma, replicateM(n - 1, ma))(_ :: _)
  /* -------- Exercise 11.6 ------- */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(Nil:List[A]))((a,b) => map2(f(a),b)((res,acc) => if(res) acc else a :: acc))
  /* -------- Exercise 11.7 ------- */
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
}

object Monads {
  implicit class MonadSyntax[A,M[_]:Monad](ma: M[A]) {
    def map[B](f: A => B): M[B] = implicitly[Monad[M]].map(ma)(f)
    def map2[B,C](mb: M[B])(f: (A,B) => C): M[C] = implicitly[Monad[M]].map2(ma,mb)(f)
    def flatMap[B](f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(ma)(f)
    def replicateM(n: Int): M[List[A]] = implicitly[Monad[M]].replicateM(n,ma)
  }

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