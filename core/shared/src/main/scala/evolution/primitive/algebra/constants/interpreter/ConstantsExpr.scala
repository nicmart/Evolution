package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class ConstantsExpr[S[_], F[_]] extends Constants[Composed[Expr[S, F, ?], S, ?], Double] {

  override def double(d: Double): Expr[S, F, S[Double]] =
    new Expr[S, F, S[Double]] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[S[Double]] =
        alg.constants.double(d)
    }

  override def point(x: Expr[S, F, S[Double]], y: Expr[S, F, S[Double]]): Expr[S, F, S[Point]] =
    new Expr[S, F, S[Point]] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[S[Point]] =
        alg.constants.point(x.run(alg), y.run(alg))
    }

  override def add[T: Semigroup](a: Expr[S, F, S[T]], b: Expr[S, F, S[T]]): Expr[S, F, S[T]] =
    new Expr[S, F, S[T]] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[S[T]] =
        alg.constants.add(a.run(alg), b.run(alg))
    }
}
