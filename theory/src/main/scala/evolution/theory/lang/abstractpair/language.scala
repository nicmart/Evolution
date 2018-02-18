package evolution.theory.lang.abstractpair

import scala.language.higherKinds

trait Lang[F[-_, +_], P[_, _]] {
  def int[E](n: Int): F[E, Int]
  def bool[E](b: Boolean): F[E, Boolean]
  def add[E](n: F[E, Int], m: F[E, Int]): F[E, Int]
  def ifElse[E, A](condition: F[E, Boolean], ifTrue: F[E, A], ifFalse: F[E, A]): F[E, A]
  def var0[E, A]: F[P[A, E], A]
  def varS[E, A, B](e: F[E, A]): F[P[B, E], A]
  def let[E, A, B](name: String, value: F[E, A])(expr: F[P[A, E], B]): F[E, B]
}

trait Term[E[_[_, _]], +A] {
  def run[F[-_, +_], P[_, _]](alg: Lang[F, P]): F[E[P], A]
}

object Builder {
  def int[E[_[_, _]]](n: Int): Term[E, Int] =
    new Term[E, Int] { def run[F[- _, + _], P[_, _]](alg: Lang[F, P]): F[E[P], Int] = alg.int(n) }
  def bool[E[_[_, _]]](b: Boolean): Term[E, Boolean] =
    new Term[E, Boolean] { def run[F[- _, + _], P[_, _]](alg: Lang[F, P]): F[E[P], Boolean] = alg.bool(b) }
  def add[E[_[_, _]]](n: Term[E, Int], m: Term[E, Int]): Term[E, Int] =
    new Term[E, Int] { def run[F[- _, + _], P[_, _]](alg: Lang[F, P]): F[E[P], Int] = alg.add(n.run(alg), m.run(alg)) }
  def ifElse[E[_[_, _]], A](condition: Term[E, Boolean], ifTrue: Term[E, A], ifFalse: Term[E, A]): Term[E, A] =
    new Term[E, A] { def run[F[- _, + _], P[_, _]](alg: Lang[F, P]): F[E[P], A] = alg.ifElse(condition.run(alg), ifTrue.run(alg), ifFalse.run(alg)) }
  def var0[E[_[_, _]], A]: Term[λ[P[_, _] => P[A, E[P]]], A] =
    new Term[λ[P[_, _] => P[A, E[P]]], A] { def run[F[- _, + _], P[_, _]](alg: Lang[F, P]): F[P[A, E[P]], A] = alg.var0 }
  def let[E[_[_, _]], A, B](name: String, value: Term[E, A])(expr: Term[λ[P[_, _] => P[A, E[P]]], B]): Term[E, B] =
    new Term[E, B] { def run[F[- _, + _], P[_, _]](alg: Lang[F, P]): F[E[P], B] = alg.let(name, value.run(alg))(expr.run(alg)) }
  def varS[E[_[_, _]], A, B](e: Term[E, A]): Term[λ[P[_, _] => P[B, E[P]]], A] =
    new Term[λ[P[_, _] => P[B, E[P]]], A] { def run[F[- _, + _], P[_, _]](alg: Lang[F, P]): F[P[B, E[P]], A] = alg.varS(e.run(alg)) }
}


