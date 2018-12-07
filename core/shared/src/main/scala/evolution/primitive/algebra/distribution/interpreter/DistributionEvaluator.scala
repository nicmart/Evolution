package evolution.primitive.algebra.distribution.interpreter

import evolution.primitive.algebra.distribution.Distribution
import evolution.data.AnnotationModule._

object DistributionEvaluator extends Distribution[F, R] {
  override def uniform(fromEval: R[Double], toEval: R[Double]): R[F[Double]] =
    Annotation(fromEval.vars ++ toEval.vars, builder.distribution.uniform(fromEval.expr, toEval.expr))
}
