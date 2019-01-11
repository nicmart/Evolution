package evolution.primitive.algebra.derived.interpreter

import evolution.geometry.Point
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.typeclass.VectorSpace

class DerivedExpr[F[_]] extends Derived[F, Expr[F, ?]] {
  override def cartesian(x: Expr[F, F[Double]], y: Expr[F, F[Double]]): Expr[F, F[Point]] =
    new Expr[F, F[Point]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[Point]] =
        alg.derived.cartesian(x.run(alg), y.run(alg))
    }

  override def constant[A](a: Expr[F, A]): Expr[F, F[A]] =
    new Expr[F, F[A]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[A]] =
        alg.derived.constant(a.run(alg))
    }

  override def concat[A](fa1: Expr[F, F[A]], fa2: Expr[F, F[A]]): Expr[F, F[A]] =
    new Expr[F, F[A]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[A]] =
        alg.derived.concat(fa1.run(alg), fa2.run(alg))
    }

  override def flatMap[A, B](fa: Expr[F, F[A]], f: Expr[F, A => F[B]]): Expr[F, F[B]] =
    new Expr[F, F[B]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[B]] =
        alg.derived.flatMap(fa.run(alg), f.run(alg))
    }

  override def polar(radius: Expr[F, F[Double]], angle: Expr[F, F[Double]]): Expr[F, F[Point]] =
    new Expr[F, F[Point]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[Point]] =
        alg.derived.polar(radius.run(alg), angle.run(alg))
    }
  override def integrate[A: VectorSpace](start: Expr[F, A], speed: Expr[F, F[A]]): Expr[F, F[A]] =
    new Expr[F, F[A]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[A]] =
        alg.derived.integrate(start.run(alg), speed.run(alg))
    }

  override def map[A, B](fa: Expr[F, F[A]], f: Expr[F, A => B]): Expr[F, F[B]] =
    new Expr[F, F[B]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[B]] =
        alg.derived.map(fa.run(alg), f.run(alg))
    }

  override def take[T](n: Expr[F, Int], ft: Expr[F, F[T]]): Expr[F, F[T]] =
    new Expr[F, F[T]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[T]] =
        alg.derived.take(n.run(alg), ft.run(alg))
    }
}
