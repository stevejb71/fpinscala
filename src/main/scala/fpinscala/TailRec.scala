package fpinscala
package tailcalls

import annotation.tailrec

sealed trait TailRec[@specialized(Int) +A] {
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
}
case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec {
  def apply[A](a: A): TailRec[A] = Return(a)

  @tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
  def suspend[A](a: => TailRec[A]): TailRec[A] = Suspend(() => ()).flatMap { _ => a }
  def compose[A, B, C](f: A => TailRec[B], g: B => TailRec[C]): A => TailRec[C] = x => suspend(f(x).flatMap(g))
}

object TailRecTest extends App {
// This stack overflows:
//  val f: Int => Int = (x: Int) => x
//  val g = List.fill(100000)(f).foldLeft(f)(_ compose _)
//  println(g(42))
  val f: Int => TailRec[Int] = Return(_)
  val g = List.fill(1000000)(f).foldLeft(f)(TailRec.compose)
  println(TailRec.run(g(42)))
}