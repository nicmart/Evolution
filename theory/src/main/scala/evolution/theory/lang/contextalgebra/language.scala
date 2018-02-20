package evolution.theory.lang.contextalgebra

import scala.language.higherKinds

trait Lang[F[_]] {
  def int(n: Int): F[Int]
  def bool(b: Boolean): F[Boolean]
  def add(n: F[Int], m: F[Int]): F[Int]
  def ifElse[A](condition: F[Boolean], ifTrue: F[A], ifFalse: F[A]): F[A]
}

trait FullLang[E, F[_, _]] extends Lang[F[E, ?]] {
  def var0[A]: F[(A, E), A]
  def varS[A, B](e: F[E, A]): F[(B, E), A]
  def let[A, B](name: String, value: F[E, A])(expr: F[(A, E), B]): F[E, B]
}

trait LangAlg[F[_, _]] {
  def get[E]: FullLang[E, F]
}

trait Term[E, A] {
  def run[F[_, _]](alg: LangAlg[F]): F[E, A]
}

class Builder[E] extends FullLang[E, Term] {
  override def int(n: Int): Term[E, Int] =
    new Term[E, Int] { override def run[F[_, _]](alg: LangAlg[F]): F[E, Int] = alg.get[E].int(n) }
  override def bool(b: Boolean): Term[E, Boolean] =
    new Term[E, Boolean] { override def run[F[_, _]](alg: LangAlg[F]): F[E, Boolean] = alg.get[E].bool(b) }
  override def add(n: Term[E, Int], m: Term[E, Int]): Term[E, Int] =
    new Term[E, Int] { override def run[F[_, _]](alg: LangAlg[F]): F[E, Int] = alg.get[E](n.run(alg), m.run(alg)) }
  override def ifElse[A](condition: Term[E, Boolean], ifTrue: Term[E, A], ifFalse: Term[E, A]): Term[E, A] =
    new Term[E, A] { override def run[F[_, _]](alg: LangAlg[F]): F[E, A] = alg.get[E].ifElse(condition.run(alg), ifTrue.run(alg), ifFalse.run(alg)) }
  override def var0[A]: Term[(A, E), A] =
    new Term[(A, E), A] { override def run[F[_, _]](alg: LangAlg[F]): F[(A, E), A] = alg.get[E].var0 }
  override def let[A, B](name: String, value: Term[E, A])(expr: Term[(A, E), B]): Term[E, B] =
    new Term[E, B] { override def run[F[_, _]](alg: LangAlg[F]): F[E, B] = alg.get[E].let(name, value.run(alg))(expr.run(alg)) }
  override def varS[A, B](e: Term[E, A]): Term[(B, E), A] =
    new Term[(B, E), A] { override def run[F[_, _]](alg: LangAlg[F]): F[(B,E), A] = alg.get[E].varS(e.run(alg)) }
}

object Builder {
  object Alg extends LangAlg[Term] {
    def get[E]: FullLang[E, Term] = new Builder[E]
  }
}


