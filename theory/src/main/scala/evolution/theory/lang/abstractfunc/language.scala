package evolution.theory.lang.abstractfunc

import scala.language.higherKinds

trait BindingLang {
  type ~>[A, B]
  type Env[A]
  def var0[A]: A ~> Env[A]
  def varS[A, B](expr: Env[A]): B ~> Env[A]
  def ap[A, B](f: Env[A ~> B])(a: Env[A]): Env[B]
  def ap2[A, B, C](f: Env[A ~> (B ~> C)])(a: Env[A], b: Env[B]): Env[C] = ap(ap(f)(a))(b)
  def fix[A](f: A ~> Env[A]): A
}

trait Lang extends BindingLang { self =>
  type F0[A]
  type C0[A]
  final type F[A] = Env[F0[A]]
  final type C[A] = Env[C0[A]]
  def next[X]: LangNext[X]
  def value[A](a: A): C[A]
  def empty[A]: F[A]
  def cons[A](a: C[A], tail: F[A]): F[A]
  def mapEmpty[A](fa1: F[A], fa2: F[A]): F[A]
  def mapCons[A, B](fa1: F[A])(f: C0[A] ~> (F0[A] ~> F[B])): F[B]

  // Fine here, we are still in the applicative domain
  def mapC[A, B](ca: C[A])(f: A => B): C[B]
  def mapF[A, B](ca: F[A])(f: A => B): F[B]
  // Not sure if this is more a zipWith rather than a map2
  def map2F[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
  def map2C[A, B, Z](fa: C[A], fb: C[B])(f: (A, B) => Z): C[Z]

  trait LangNext[X] extends Lang {
    final type Env[A] = X ~> self.Env[A]
    final type ~>[A, B] = self.~>[A, B]
    final type F0[A] = self.F0[A]
    final type C0[A] = self.C0[A]
    val x: Env[X] = self.var0[X]
    def up[A](a: self.Env[A]): Env[A] = self.varS[A, X](a)
  }
}

object Lang {
  trait Base extends Lang {
    type Env[A] = A
  }
}

class Expressions(val lang: Lang.Base) {
  import lang._

  def constant[A](a: A): F[A] = {
    val b1 = next[F0[A]]
    fix(b1.cons[A](varS(value(a)), b1.x))
  }
}

trait Serializer extends Lang {
  type ~>[A, B] = A => B
  type F0[A] = String
  def var0[E, A]: ((A, E)) => A = { case (a, e) => a }
  def varS[E, A, B](expr: E => A): ((B, E)) => A = { case (b, e) => expr(e) }
  def fix[E, A](f: (A, E) => A): E => A =
    e => f(fix(f)(e), e)
  def empty[E, A]: E => String =
    _ => "empty"
  def cons[E, A](a: A, tail: E => String): E => String =
    e => s"cons(${a.toString}, ${tail(e)})"
}
