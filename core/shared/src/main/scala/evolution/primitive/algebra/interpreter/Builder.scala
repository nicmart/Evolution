package evolution.primitive.algebra.interpreter

import cats.data.NonEmptyList
import evolution.primitive.algebra.{DrawingExpr, _}
import _root_.evolution.geometry.Point

object Builder {
  def double(d: Double): ScalarExpr[Double] = new ScalarExpr[Double] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): S[Double] = alg.scalar.double(d)
  }

  def point(p: Point): ScalarExpr[Point] = new ScalarExpr[Point] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): S[Point] = alg.scalar.point(p)
  }

  def empty[A]: DrawingExpr[A] = new DrawingExpr[A] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A] =
      alg.drawing.empty
  }

  def cons[A](head: ScalarExpr[A], tail: DrawingExpr[A]): DrawingExpr[A] = new DrawingExpr[A] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A] =
      alg.drawing.cons(head.run(alg), tail.run(alg))
  }

  def mapEmpty[A](eva: DrawingExpr[A])(eva2: DrawingExpr[A]): DrawingExpr[A] = new DrawingExpr[A] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A] = alg.drawing.mapEmpty(eva.run(alg))(eva2.run(alg))
  }
  def mapCons[A, B](eva: DrawingExpr[A])(f: DrawingExpr[B]): DrawingExpr[B] = new DrawingExpr[B] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[B] = alg.drawing.mapCons(eva.run(alg))(f.run(alg))
  }

  def const[A](a: ScalarExpr[A]): DrawingExpr[A] = new DrawingExpr[A] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A] =
      alg.drawing.cons(a.run(alg), alg.bindF.var0)
  }

  def var0[A]: DrawingExpr[A] = new DrawingExpr[A] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A] =
      alg.bindF.var0[A]
  }
  def shift[A](expr: DrawingExpr[A]): DrawingExpr[A] = new DrawingExpr[A] {
    override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A] =
      alg.bindF.shift[A](expr.run(alg))
  }

  def let[In, Out](name: String, value: DrawingExpr[In])(expr: DrawingExpr[Out]): DrawingExpr[Out] =
    new DrawingExpr[Out] {
      override def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[Out] =
        alg.bindF.let[In, Out](name, value.run(alg))(expr.run(alg))
    }
}
