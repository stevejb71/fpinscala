package fpinscala

import fpinscala.Monoids.Foldable

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

//noinspection NoReturnTypeForImplicitDef
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

  type Const[A,B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M,x]})#f] {
    override def unit[A](a: => A): M = M.zero
    override def map2[A, B, C](a: M, b: M)(f: (A, B) => C): M = M.op(a,b)
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

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]: Applicative,A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
  /* ------- Exercise 12.14 -------- */
  import Monads._
  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id,A,B](fa)(a => Id(f(a)))(Monads.Id.idMonad).value

  import Applicatives._
  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = M})#f,A,Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monads.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A,S) => (B,S)): (F[B],S) =
    traverseS(fa)(a => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  /* -------- Exercise 12.16 -------- */
  override def toList[A](fa: F[A]): List[A] = mapAccum(fa, Nil:List[A])((a,s) => ((),a::s))._2.reverse

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa,toList(fa).reverse)((a,as) => (as.head,as.tail))._1

  /* -------- Exercise 12.17 -------- */
  def foldLeftViaMapAccum[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as,z)((a,b) => ((),f(b,a)))._2

  /* -------- Exercise 12.18 -------- */
  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B],g: A => H[B])(G: Applicative[G],H:Applicative[H]): (G[F[B]],H[F[B]]) = {
    type Prod[x] = (G[x], H[x])
    traverse[Prod,A,B](fa)(a => (f(a),g(a)))(G product H)
  }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

/* ------- Exercise 12.13 -------- */
object Traverse {
  val listTraverse = new Traverse[List] {
    override def sequence[G[_] : Applicative, A](fga: List[G[A]]): G[List[A]] = {
      val GA = implicitly[Applicative[G]]
      fga.foldRight(GA.unit(Nil:List[A]))((ga,gas) => GA.map2(ga,gas)(_ :: _))
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def sequence[G[_] : Applicative, A](fga: Option[G[A]]): G[Option[A]] = {
      val GA = implicitly[Applicative[G]]
      fga.fold(GA.unit(None:Option[A]))(ga => GA.map(ga)(Some(_)))
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      val h = f(ta.head)
      val t = listTraverse.traverse(ta.tail)(traverse(_)(f))
      G.map2(h, t)(Tree(_,_))
    }
  }
}