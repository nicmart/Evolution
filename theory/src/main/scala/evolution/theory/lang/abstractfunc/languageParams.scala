package evolution.theory.lang.abstractfunc.params

import scala.language.higherKinds

trait BindingLang[~>[_, _]] {
  def var0[A]: A ~> A
  def varS[A, B](expr: A): B ~> A
  def ap[A, B](f: A ~> B)(a: A): B
  def ap2[A, B, C](f: A ~> (B ~> C))(a: A, b: B): C = ap(ap(f)(a))(b)
  def fix[A](f: A ~> A): A
}

// 1) A simple unityped language for integers, without explicit environment
trait Lang[~>[_, _], T] extends BindingLang[~>] { self =>
  def int(n: Int): T
  def sum(a: T, b: T): T
  def diff(a: T, b: T): T
  def mul(a: T, b: T): T
  def ifLessOrEqualThan(p: T)(a: T, b: T): T
  def next[A]: Lang[~>, A ~> T]
}

class IntExpressions[~>[_, _], E[_], T](val lang: Lang[~>, T]) {
  import lang._
  def factorial(n: T): T = {
    val b1 = lang.next[T ~> T]
    val b2 = b1.next[T]
    val rec = var0[T ~> T]
    ap(
      fix[T ~> T](
        ???
        //b.ifLessOrEqualThan(b.int(0))(b.int(1), b.mul(???, ???))
      )
    )(n)
  }
}

// 2) A simple language for lists, with map and flatmap

object Lang {
  type EnvBase[A] = A
  type Id[A] = A
}