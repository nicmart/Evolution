package evolution.theory.lang.list

import evolution.theory.lang.list.Expr.{ExprF, Next}

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

  def mapC[A, B](ca: C[A])(f: A => B): C[B]
  def mapF[A, B](ca: F[A])(f: A => B): F[B]
}

object Lang {
  type Aux[FF[_], CC[_], ~~>[_, _]] = Lang {
    type F[A] = FF[A]
    type C[A] = CC[A]
    type ~>[A, B] = ~~>[A, B]
  }
}

trait Expr[A[F[_], C[_], ~>[_, _]]] { self =>
  import Expr._
  def run(alg: Lang): alg.Apply[A]
  def next[E[F[_], C[_], ~>[_, _]]]: Expr[Next[E, A]#T] = new Expr[Next[E, A]#T] {
    override def run(alg: Lang): alg.Apply[Next[E, A]#T] =
      self.run(alg.next[alg.Apply[E]])
  }
}

object Expr {
  trait T[A] {
    final type FT[F[_], C[_], ~>[_, _]] = F[A]
    final type CT[F[_], C[_], ~>[_, _]] = C[A]
    final type Self[F[_], C[_], ~>[_, _]] = A
    trait Next[From[F[_], C[_], ~>[_, _]]] {
      final type T[F[_], C[_], ~>[_, _]] = F[A] ~> From[F, C, ~>]
    }
  }
  trait Next[From[F[_], C[_], ~>[_, _]], To[F[_], C[_], ~>[_, _]]] {
    final type T[F[_], C[_], ~>[_, _]] = To[λ[X => From[F, C, ~>] ~> F[X]], λ[X => From[F, C, ~>] ~> C[X]], ~>]
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

class Expressions(val lang: Lang) {
  import lang._
  import Expr._

  def nextExpressions[X]: Expressions = new Expressions(lang.next[X])

  def constant[A](a: A): F[A] =
    fix(next[F[A]].cons[A](varS(value(a)), var0))

  def integrate(v: F[Double]): C[Double] ~> F[Double] = {
    val b1 = next[C[Double]]
    val b2 = b1.next[C[Double] ~> F[Double]]
    val b3 = b2.next[C[Double] ~> F[Double]]
    // Inside fix we need (C[D] ~> F[D]) ~> (C[D] ~> F[D])
    // Inside fix we need (C[D] ~> F[D]) ~> C[D] ~> F[D]
    // The F[] for mapCons must then be (C[D] ~> F[D]) ~> C[D] ~> F[_]
    // That means we first push C[D], and then (C[D] ~> F[D])
    fix[C[Double] ~> F[Double]](b2.mapCons[Double, Double](???)(???))
  }
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