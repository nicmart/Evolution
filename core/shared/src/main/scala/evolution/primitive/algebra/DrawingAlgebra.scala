package evolution.primitive.algebra

import _root_.evolution.geometry.Point
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import cats.~>

trait BindingAlgebra[R[_], VarName] {
  def varName(name: String): VarName
  def var0[A]: R[A]
  def shift[A](expr: R[A]): R[A]
  def let[A, B](name: VarName, value: R[A])(expr: R[B]): R[B]
  def lambda[A, B](name: VarName, expr: R[B]): R[A => B]
  def app[A, B](f: R[A => B], a: R[A]): R[B]
  def fix[A](expr: R[A => A]): R[A]
}

trait CoreDrawingAlgebra[S[_], F[_], R[_]] {
  def empty[A]: R[F[A]]
  def cons[A](head: R[S[A]], tail: R[F[A]]): R[F[A]]
  def mapEmpty[A](eva: R[F[A]])(eva2: R[F[A]]): R[F[A]]
  def mapCons[A, B](eva: R[F[A]])(f: R[S[A] => F[A] => F[B]]): R[F[B]]
}

trait ScalarAlgebra[S[_]] {
  def double(d: Double): S[Double]
  def point(x: Double, y: Double): S[Point]
  def add[T: Semigroup](a: S[T], b: S[T]): S[T]
}

class MappedBindingAlgebra[R1[_], R2[_], VarName](alg: BindingAlgebra[R1, VarName], to: R1 ~> R2, from: R2 ~> R1)
    extends BindingAlgebra[R2, VarName] {
  def varName(name: String): VarName =
    alg.varName(name)
  def var0[A]: R2[A] =
    to(alg.var0)
  def shift[A](expr: R2[A]): R2[A] =
    to(alg.shift(from(expr)))
  def let[A, B](name: VarName, value: R2[A])(expr: R2[B]): R2[B] =
    to(alg.let(name, from(value))(from(expr)))
  def lambda[A, B](name: VarName, expr: R2[B]): R2[A => B] =
    to(alg.lambda(name, from(expr)))
  def app[A, B](f: R2[A => B], a: R2[A]): R2[B] =
    to(alg.app(from(f), from(a)))
  def fix[A](expr: R2[A => A]): R2[A] =
    to(alg.fix(from(expr)))
}

class MappedCoreDrawingAlgebra[S[_], F[_], R1[_], R2[_]](
  alg: CoreDrawingAlgebra[S, F, R1],
  to: R1 ~> R2,
  from: R2 ~> R1
) extends CoreDrawingAlgebra[S, F, R2] {
  def empty[A]: R2[F[A]] =
    to(alg.empty)
  def cons[A](head: R2[S[A]], tail: R2[F[A]]): R2[F[A]] =
    to(alg.cons(from(head), from(tail)))
  def mapEmpty[A](eva: R2[F[A]])(eva2: R2[F[A]]): R2[F[A]] =
    to(alg.mapEmpty(from(eva))(from(eva2)))
  def mapCons[A, B](eva: R2[F[A]])(f: R2[S[A] => F[A] => F[B]]): R2[F[B]] =
    to(alg.mapCons(from(eva))(from(f)))
}

class ContextualCoreDrawingAlgebra[S[_], F[_], R[_], Ctx](alg: CoreDrawingAlgebra[S, F, R])
    extends CoreDrawingAlgebra[S, F, λ[α => Ctx => R[α]]] {
  override def empty[A]: Ctx => R[F[A]] = _ => alg.empty
  override def cons[A](head: Ctx => R[S[A]], tail: Ctx => R[F[A]]): Ctx => R[F[A]] =
    ctx => alg.cons(head(ctx), tail(ctx))
  override def mapEmpty[A](eva: Ctx => R[F[A]])(eva2: Ctx => R[F[A]]): Ctx => R[F[A]] =
    ctx => alg.mapEmpty(eva(ctx))(eva2(ctx))
  override def mapCons[A, B](eva: Ctx => R[F[A]])(f: Ctx => R[S[A] => F[A] => F[B]]): Ctx => R[F[B]] =
    ctx => alg.mapCons(eva(ctx))(f(ctx))
}

class MappedScalarAlgebra[S1[_], S2[_]](alg: ScalarAlgebra[S1], to: S1 ~> S2, from: S2 ~> S1)
    extends ScalarAlgebra[S2] {
  def double(d: Double): S2[Double] =
    to(alg.double(d))
  def point(x: Double, y: Double): S2[Point] =
    to(alg.point(x, y))
  def add[T: Semigroup](a: S2[T], b: S2[T]): S2[T] =
    to(alg.add(from(a), from(b)))
}

class ContextualScalarAlgebra[S[_], Ctx](alg: ScalarAlgebra[S]) extends ScalarAlgebra[λ[α => Ctx => S[α]]] {
  override def double(d: Double): Ctx => S[Double] = _ => alg.double(d)
  override def point(x: Double, y: Double): Ctx => S[Point] = _ => alg.point(x, y)
  override def add[T: Semigroup](a: Ctx => S[T], b: Ctx => S[T]): Ctx => S[T] =
    ctx => alg.add(a(ctx), b(ctx))
}
