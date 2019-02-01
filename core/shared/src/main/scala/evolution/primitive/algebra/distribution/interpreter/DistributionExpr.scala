package evolution.primitive.algebra.distribution.interpreter
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class DistributionExpr[F[_]] extends Distribution[F, Expr[F, ?]] {
  override def uniform(from: Expr[F, Double], to: Expr[F, Double]): Expr[F, F[Double]] =
    new Expr[F, F[Double]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[Double]] =
        alg.distribution.uniform(from.run(alg), to.run(alg))
    }

  override def uniformDiscrete(from: Expr[F, Double], to: Expr[F, Double], step: Expr[F, Double]): Expr[F, F[Double]] =
    new Expr[F, F[Double]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[Double]] =
        alg.distribution.uniformDiscrete(from.run(alg), to.run(alg), step.run(alg))
    }

  override def uniformChoice[T](ts: List[Expr[F, T]]): Expr[F, F[T]] =
    new Expr[F, F[T]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[T]] =
        alg.distribution.uniformChoice(ts.map(_.run(alg)))
    }
}
