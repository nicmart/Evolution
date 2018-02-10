package evolution.theory.lang.higherorder

import scala.language.higherKinds

trait Lang[F[-_, +_]] {
  def int[E](n: Int): F[E, Int]
  def bool[E](b: Boolean): F[E, Boolean]
  def add[E](n: F[E, Int], m: F[E, Int]): F[E, Int]
  def ifElse[E, A](condition: F[E, Boolean], ifTrue: F[E, A], ifFalse: F[E, A]): F[E, A]
  def var0[E, A]: F[(A, E), A]
  def varS[E, A, B](e: F[E, A]): F[(B, E), A]
  def let[E, A, B](name: String, value: F[E, A])(expr: F[(A, E), B]): F[E, B]
}

trait Term[-E, +A] {
  def run[F[-_, +_]](alg: Lang[F]): F[E, A]
}

object Builder extends Lang[Term] {
  override def int[E](n: Int): Term[E, Int] =
    new Term[E, Int] { override def run[F[- _, + _]](alg: Lang[F]): F[E, Int] = alg.int(n) }
  override def bool[E](b: Boolean): Term[E, Boolean] =
    new Term[E, Boolean] { override def run[F[- _, + _]](alg: Lang[F]): F[E, Boolean] = alg.bool(b) }
  override def add[E](n: Term[E, Int], m: Term[E, Int]): Term[E, Int] =
    new Term[E, Int] { override def run[F[- _, + _]](alg: Lang[F]): F[E, Int] = alg.add(n.run(alg), m.run(alg)) }
  override def ifElse[E, A](condition: Term[E, Boolean], ifTrue: Term[E, A], ifFalse: Term[E, A]): Term[E, A] =
    new Term[E, A] { override def run[F[- _, + _]](alg: Lang[F]): F[E, A] = alg.ifElse(condition.run(alg), ifTrue.run(alg), ifFalse.run(alg)) }
  override def var0[E, A]: Term[(A, E), A] =
    new Term[(A, E), A] { override def run[F[- _, + _]](alg: Lang[F]): F[(A, E), A] = alg.var0 }
  override def let[E, A, B](name: String, value: Term[E, A])(expr: Term[(A, E), B]): Term[E, B] =
    new Term[E, B] { override def run[F[- _, + _]](alg: Lang[F]): F[E, B] = alg.let(name, value.run(alg))(expr.run(alg)) }
  override def varS[E, A, B](e: Term[E, A]): Term[(B, E), A] =
    new Term[(B, E), A] { override def run[F[- _, + _]](alg: Lang[F]): F[(B,E), A] = alg.varS(e.run(alg)) }
}


