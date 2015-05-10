package fpinscala

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(Nil: List[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /* ------- Exercise 12.1 -------- */

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(ma, replicateM(n - 1, ma))(_ :: _)
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((a, b) => (a, b))

  /* ------- Exercise 12.2 (also above in definition of map, map2) -------- */
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

  /* ------- Exercise 12.3 -------- */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(map(fa)(f.curried))(fb))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  /* ------- Exercise 12.8 -------- */
  def product[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    private val base = Applicative.this
    override def unit[A](a: => A): (F[A], G[A]) = (base.unit(a), G.unit(a))
    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
      (base.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
  }

  /* ------- Exercise 12.9 -------- */
  def compose[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = F[G[x]]})#f] {
    private val base = Applicative.this
    override def unit[A](a: => A): F[G[A]] = base.unit(G.unit(a))
    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = base.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
  }

  /* ------- Exercise 12.12 -------- */
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map.empty[K, V])) {
    case (m,(k,fv)) => map2(m, fv)((m,v) => m + (k -> v))
  }
}

object Applicatives {
  implicit val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.continually(a)
    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] = (fa zip fb) map f.tupled
  }
  implicit val listApplicative = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = for {
      a <- fa
      b <- fb
    } yield f(a,b)
  }
  implicit val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = for {
      a <- fa
      b <- fb
    } yield f(a,b)
  }
}

/* ------- Exercise 12.6 -------- */
sealed trait Validation[+E,+A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E,Nothing]
case class Success[A](a: A) extends Validation[Nothing,A]

object Validation {
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a),Success(b)) => Success(f(a,b))
      case (Failure(ha,ta),Failure(hb,tb)) => Failure(ha, ta ++ Vector(hb) ++ tb)
      case (Failure(h,t),_) => Failure(h,t)
      case (_,Failure(h,t)) => Failure(h,t)
    }
  }
}

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]: Applicative,A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

/* ------- Exercise 12.13 -------- */
object Traverse {
  val listTraverse = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
    override def sequence[G[_] : Applicative, A](fga: List[G[A]]): G[List[A]] = {
      val GA = implicitly[Applicative[G]]
      fga.foldRight(GA.unit(Nil:List[A]))((ga,gas) => GA.map2(ga,gas)(_ :: _))
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
    override def sequence[G[_] : Applicative, A](fga: Option[G[A]]): G[Option[A]] = {
      val GA = implicitly[Applicative[G]]
      fga.fold(GA.unit(None:Option[A]))(ga => GA.map(ga)(Some(_)))
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = Tree(f(fa.head),fa.tail map f)
    override def sequence[G[_] : Applicative, A](fga: Tree[G[A]]): G[Tree[A]] = ???
  }
}

