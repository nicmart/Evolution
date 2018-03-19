package evolution.theory.lang.list

import evolution.theory.lang.list.Expr.{ExprF, Fn}

trait BindingLang {
  type ~>[A, B]
  def var0[A]: A ~> A
  def varS[A, B](expr: A): B ~> A
  def fix[A](f: A ~> A): A
}

trait Lang extends BindingLang { self =>
  type F[A]
  type C[A]
  type ~>[A, B]
  def next[X]: Lang.Aux[λ[T => X ~> F[T]], λ[T => X ~> C[T]], ~>]
  final type Apply[T[F[_], C[_], ~>[_, _]]] = T[F, C, ~>]
  def value[A](a: A): C[A]
  def empty[A]: F[A]
  def cons[A](a: C[A], tail: F[A]): F[A]
  def mapEmpty[A](fa1: F[A], fa2: F[A]): F[A]
  def mapCons[A, B](fa1: F[A])(f: C[A] ~> F[A] ~> F[B]): F[B]
}

object Lang {
  type Aux[FF[_], CC[_], ~~>[_, _]] = Lang {
    type F[A] = FF[A]
    type C[A] = CC[A]
    type ~>[A, B] = ~~>[A, B]
  }
}

trait Expr[A[F[_], C[_], ~>[_, _]]] { self =>
  def run(alg: Lang): alg.Apply[A]
  def next[E[F[_], C[_], ~>[_, _]]]: Expr[Fn[E, A]#T] = new Expr[Fn[E, A]#T] {
    override def run(alg: Lang): alg.~>[alg.Apply[E], alg.Apply[A]] =
      ???
      //self.run(alg.next[alg.Apply[E]])
  }
}

object Expr {
  trait T[A] {
    final type FT[F[_], C[_], ~>[_, _]] = F[A]
    final type CT[F[_], C[_], ~>[_, _]] = C[A]
    final type Self[F[_], C[_], ~>[_, _]] = A
  }
  trait Fn[From[F[_], C[_], ~>[_, _]], To[F[_], C[_], ~>[_, _]]] {
    final type T[F[_], C[_], ~>[_, _]] = From[F, C, ~>] ~> To[F, C, ~>]
  }

  trait ExprPlain[A] extends Expr[T[A]#Self]
  trait ExprF[A] extends Expr[T[A]#FT]
  trait ExprC[A] extends Expr[T[A]#CT]
  trait ExprFnF[A, B] extends Expr[Fn[T[A]#FT, T[B]#FT]#T]
  trait ExprFn[A, B] extends Expr[Fn[T[A]#Self, T[B]#Self]#T]
}

trait ValueExpr[A] {
  def run(alg: Lang): alg.C[A]
}

class Expressions {
  import Expr._

  def value[A](a: A): ExprC[A] = new ExprC[A] {
    override def run(alg: Lang): alg.C[A] = alg.value(a)
  }

  def cons[A](a: ExprC[A], tail: ExprF[A]): ExprF[A] =
    new ExprF[A] {
      override def run(alg: Lang): alg.F[A] =
        alg.cons(a.run(alg), tail.run(alg))
    }

  def var0[A]: ExprFn[A, A] =
    new ExprFn[A, A] {
      override def run(alg: Lang): alg.Apply[Fn[T[A]#Self, T[A]#Self]#T] =
        alg.var0
    }

  def fix[A](expr: ExprFn[A, A]): ExprPlain[A] =
    new ExprPlain[A] {
      override def run(alg: Lang): A =
        alg.fix(expr.run(alg))
    }

  def mapEmpty[A](fa1: ExprF[A], fa2: ExprF[A]): ExprF[A] =
    new ExprF[A] {
      override def run(alg: Lang): alg.F[A] =
        alg.mapEmpty(fa1.run(alg), fa2.run(alg))
    }
//
//  def mapCons[A, B](fa1: Expr[E, A])(f: ExprCons[A, B]): Expr[E, B] =
//    new Expr[E, B] {
//      override def run(alg: Lang): alg.~>[E[alg.F], alg.F[B]] =
//        alg.mapCons(fa1.run(alg))(f.run(alg))
//    }
//
//  def constant[A](a: A): ExprF[A] =
//    fix(next[A].cons(a, var0))
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