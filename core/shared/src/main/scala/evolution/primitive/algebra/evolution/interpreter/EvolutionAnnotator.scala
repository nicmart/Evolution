package evolution.primitive.algebra.evolution.interpreter

import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingAnnotator
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainAnnotator
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsAnnotator
import evolution.primitive.algebra.derived.{ DefaultDerived, Derived }
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.distribution.interpreter.DistributionAnnotator
import evolution.primitive.algebra.evolution.Evolution
import evolution.data.AnnotationModule._

// Generic TODO: Make sure we do as much as possible outside the closures
object EvolutionAnnotator extends Evolution[F, R] {
  override val chain: Chain[F, R] = ChainAnnotator
  override val constants: Constants[R] = ConstantsAnnotator
  override val bind: Binding[R, String] = BindingAnnotator
  override val distribution: Distribution[F, R] = DistributionAnnotator
  override val derived: Derived[F, R] = new DefaultDerived(this)
}
