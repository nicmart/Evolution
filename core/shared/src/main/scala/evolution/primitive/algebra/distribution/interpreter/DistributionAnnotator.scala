package evolution.primitive.algebra.distribution.interpreter

import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation.Info.StatelessEvolution
import evolution.data.Annotation
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr

object DistributionAnnotator extends Distribution[RNGRepr, Annotation] {
  private val builder = new EvolutionExpr[RNGRepr]
  override def uniform(fromEval: Annotation[Double], toEval: Annotation[Double]): Annotation[RNGRepr[Double]] =
    Annotation(
      fromEval.vars ++ toEval.vars,
      StatelessEvolution(builder.distribution.uniform(fromEval.expr, toEval.expr))
    )
  override def uniformChoice[T](tsEval: List[Annotation[T]]): Annotation[RNGRepr[T]] =
    Annotation(
      tsEval.foldLeft(Set.empty[Int]) { (vars, annotation) =>
        vars ++ annotation.vars
      },
      StatelessEvolution(builder.distribution.uniformChoice(tsEval.map(_.expr)))
    )
}
