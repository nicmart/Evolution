package evolution.primitive.algebra.distribution.interpreter
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class DistributionExpr[F[_]] extends Distribution[F, Expr[F, ?]] {
  override def uniform(from: Expr[F, Double], to: Expr[F, Double]): Expr[F, F[Double]] =
    new Expr[F, F[Double]] {
      override def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[F[Double]] =
        alg.distribution.uniform(from.run(alg), to.run(alg))
    }
}
