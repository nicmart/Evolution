package evolution.primitive.algebra.derived.interpreter

import evolution.geometry.Point
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class DerivedExpr[F[_]] extends Derived[F, Expr[F, ?]] {
  override def cartesian(x: Expr[F, F[Double]], y: Expr[F, F[Double]]): Expr[F, F[Point]] =
    new Expr[F, F[Point]] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[F[Point]] =
        alg.derived.cartesian(x.run(alg), y.run(alg))
    }

  override def constant[A](a: Expr[F, A]): Expr[F, F[A]] =
    new Expr[F, F[A]] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[F[A]] =
        alg.derived.constant(a.run(alg))
    }
  override def polar(radius: Expr[F, F[Double]], angle: Expr[F, F[Double]]): Expr[F, F[Point]] =
    new Expr[F, F[Point]] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[F[Point]] =
        alg.derived.polar(radius.run(alg), angle.run(alg))
    }
}
