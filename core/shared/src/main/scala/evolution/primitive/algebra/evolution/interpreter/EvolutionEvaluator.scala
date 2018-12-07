package evolution.primitive.algebra.evolution.interpreter

import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainEvaluator
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsEvaluator
import evolution.primitive.algebra.derived.{ DefaultDerived, Derived }
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.distribution.interpreter.DistributionEvaluator
import evolution.primitive.algebra.evolution.Evolution
import evolution.data.AnnotationModule._

// Generic TODO: Make sure we do as much as possible outside the closures
object EvolutionEvaluator extends Evolution[F, R] {
  override val chain: Chain[F, R] = ChainEvaluator
  override val constants: Constants[R] = ConstantsEvaluator
  override val bind: Binding[R, String] = BindingEvaluator
  override val distribution: Distribution[F, R] = DistributionEvaluator
  override val derived: Derived[F, R] = new DefaultDerived(this)
}
