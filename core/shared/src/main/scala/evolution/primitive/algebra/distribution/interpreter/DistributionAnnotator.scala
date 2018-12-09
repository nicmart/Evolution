package evolution.primitive.algebra.distribution.interpreter

import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr

object DistributionAnnotator extends Distribution[RNGRepr, Annotation] {
  private val builder = new EvolutionExpr[RNGRepr]
  override def uniform(fromEval: Annotation[Double], toEval: Annotation[Double]): Annotation[RNGRepr[Double]] =
    Annotation(fromEval.vars ++ toEval.vars, builder.distribution.uniform(fromEval.expr, toEval.expr))
}
