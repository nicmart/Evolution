package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class ConstantsExpr[F[_]] extends Constants[Expr[F, ?], Double] {

  override def double(d: Double): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[Double] =
        alg.constants.double(d)
    }

  override def point(x: Expr[F, Double], y: Expr[F, Double]): Expr[F, Point] =
    new Expr[F, Point] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[Point] =
        alg.constants.point(x.run(alg), y.run(alg))
    }

  override def add[T: Semigroup](a: Expr[F, T], b: Expr[F, T]): Expr[F, T] =
    new Expr[F, T] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[T] =
        alg.constants.add(a.run(alg), b.run(alg))
    }
  override def sin(d: Expr[F, Double]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[Double] =
        alg.constants.sin(d.run(alg))
    }

  override def cos(d: Expr[F, Double]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[Double] =
        alg.constants.cos(d.run(alg))
    }
}
