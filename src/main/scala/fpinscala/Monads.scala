package fpinscala

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]
  /* -------- Exercise 11.8 ------- */
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_:Unit) => ma, f)(())
  // map, map2 moved to Applicative
  /* -------- Exercise 11.3 ------- */
  // Moved to Applicative
  /* -------- Exercise 11.4 ------- */
  // Moved to Applicative
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

  /* -------- Exercise 11.17 ------- */

  case class Id[A](value: A) extends AnyVal

  object Id {
    implicit val idMonad = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)
      override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)
    }
  }

  /* -------- Exercise 11.20 ------- */

  case class Reader[R,A](run: R => A) extends AnyVal {
    def map[B](f: A => B): Reader[R,B] = Reader.readerMonad.map(this)(f)
    def flatMap[B](f: A => Reader[R,B]): Reader[R,B] = Reader.readerMonad.flatMap(this)(f)
  }

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
      override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(ma.run(r)).run(r))
    }
  }
}