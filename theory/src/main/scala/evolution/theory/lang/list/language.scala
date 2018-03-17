package evolution.theory.lang.list

trait BindingLang {
  type ~>[A, B]
  def var0[E, A]: (A, E) ~> A
  def varS[E, A, B](expr: E ~> A): (B, E) ~> A
  def fix[E, A](f: (A, E) ~> A): E ~> A
}

trait Lang extends BindingLang {
  type F[A]
  def empty[E, A]: E ~> F[A]
  def cons[E, A](a: A, tail: E ~> F[A]): E ~> F[A]
  def mapEmpty[E, A](fa1: E ~> F[A], fa2: E ~> F[A]): E ~> F[A]
  def mapCons[E, A, B](fa1: E ~> F[A])(f: (A, (F[A], E)) ~> F[B]): E ~> F[B]
}

trait Expr[E[_[_]], A] {
  def run(alg: Lang): alg.~>[E[alg.F], alg.F[A]]
}

class Expressions[E[_[_]]] {
  type Next[A, F[_]] = (F[A], E[F])
  type NextC[A, F[_]] = (A, E[F])
  type NextCons[A, F[_]] = (A, (F[A], E[F]))
  type ExprS[A, B] = Expr[Next[A, ?[_]], B]
  type ExprSC[A, B] = Expr[NextC[A, ?[_]], B]
  type ExprCons[A, B] = Expr[NextCons[A, ?[_]], B]
  def next[A]: Expressions[Next[A, ?[_]]] =
    new Expressions[Next[A, ?[_]]]

  def cons[A](a: A, tail: Expr[E, A]): Expr[E, A] =
    new Expr[E, A] {
      override def run(alg: Lang): alg.~>[E[alg.F], alg.F[A]] =
        alg.cons(a, tail.run(alg))
    }

  def var0[A]: Expr[Next[A, ?[_]], A] =
    new Expr[Next[A, ?[_]], A] {
      override def run(alg: Lang): alg.~>[(alg.F[A], E[alg.F]), alg.F[A]] =
        alg.var0
    }

  def fix[A](expr: Expr[Next[A, ?[_]], A]): Expr[E, A] =
    new Expr[E, A] {
      override def run(alg: Lang): alg.~>[E[alg.F], alg.F[A]] =
        alg.fix(expr.run(alg))
    }

  def mapEmpty[A](fa1: Expr[E, A], fa2: Expr[E, A]): Expr[E, A] =
    new Expr[E, A] {
      override def run(alg: Lang): alg.~>[E[alg.F], alg.F[A]] =
        alg.mapEmpty(fa1.run(alg), fa2.run(alg))
    }

  def mapCons[A, B](fa1: Expr[E, A])(f: ExprCons[A, B]): Expr[E, B] =
    new Expr[E, B] {
      override def run(alg: Lang): alg.~>[E[alg.F], alg.F[B]] =
        alg.mapCons(fa1.run(alg))(f.run(alg))
    }

  def constant[A](a: A): Expr[E, A] =
    fix(next[A].cons(a, var0))
}

trait Serializer extends Lang {
  type ~>[A, B] = A => B
  type F[A] = String
  def var0[E, A]: ((A, E)) => A =
    { case (a, e) => a }
  def varS[E, A, B](expr: E => A): ((B, E)) => A =
    { case (b, e) => expr(e) }
  def fix[E, A](f: (A, E) => A): E => A =
    e => f(fix(f)(e), e)
  def empty[E, A]: E => String =
    _ => "empty"
  def cons[E, A](a: A, tail: E => String): E => String =
    e => s"cons(${a.toString}, ${tail(e)})"
}