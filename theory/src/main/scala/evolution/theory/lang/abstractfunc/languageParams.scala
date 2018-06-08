package evolution.theory.lang.abstractfunc.params

import scala.language.higherKinds

trait BindingLang[~>[_, _], E[_]] {
  def var0[A]: A ~> E[A]
  def varS[A, B](expr: E[A]): B ~> E[A]
  def ap[A, B](f: A ~> E[B])(a: A): E[B] // Shall we wrap all the function into a single E?
  def fix[A](f: A ~> E[A]): E[A]
}

trait Lang[T] { self =>
  def int(n: Int): T
  def sum(a: T, b: T): T
}

trait LangContext[T, ~>[_, _], E[_]] {
  def lang: Lang[E[T]]
  def binding: BindingLang[~>, E]
  def next[A]: LangContext[A ~> E[T], ~>, λ[X => A ~> E[X]]]
}

object LangEvaluators {
  val eval: Lang[Int] = new Lang[Int] {
    override def int(n: Int): Int = n
    override def sum(a: Int, b: Int): Int = a + b
  }

  def lift[T, U](l: Lang[T]): Lang[U => T] = new Lang[U => T] {
    override def int(n: Int): U => T = _ => l.int(n)
    override def sum(a: U => T, b: U => T): U => T = u => l.sum(a(u), b(u))
  }
}

object BindingLangEvaluators {
  type Id[T] = T
  val funcEval: BindingLang[Function1, Id] = new BindingLang[Function1, Id] {
    override def var0[A]: A => Id[A] = identity
    override def varS[A, B](expr: Id[A]): B => Id[A] = _  => expr
    override def ap[A, B](f: A => Id[B])(a: A): Id[B] = f(a)
    override def fix[A](f: A => Id[A]): Id[A] = f(fix(f))
  }

  def lift[E[_], T](bindingLang: BindingLang[Function1, E]): BindingLang[Function1, λ[X => T => E[X]]] =
    new BindingLang[Function1, λ[X => T => E[X]]] {
      override def var0[A]: A => (T => E[A]) = varS(bindingLang.var0[A])
      override def varS[A, B](expr: T => E[A]): B => (T => E[A]) = _ => expr
      override def ap[A, B](f: A => (T => E[B]))(a: A): T => E[B] = t => bindingLang.ap[A, B](aa => f(aa)(t))(a)
      override def fix[A](f: A => (T => E[A])): T => E[A] = t => bindingLang.fix[A](aa => f(aa)(t))
    }
}



class IntExpressions[~>[_, _], E[_], T](val langContext: LangContext[T, ~>, E]) {
  def factorial(n: T): T = ???
}