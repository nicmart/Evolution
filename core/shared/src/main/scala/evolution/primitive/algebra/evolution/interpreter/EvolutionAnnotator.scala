package evolution.primitive.algebra.evolution.interpreter

import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation
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

// Generic TODO: Make sure we do as much as possible outside the closures
object EvolutionAnnotator extends Evolution[RNGRepr, Annotation] {
  override val chain: Chain[RNGRepr, Annotation] = ChainAnnotator
  override val constants: Constants[Annotation] = ConstantsAnnotator
  override val bind: Binding[Annotation, String] = BindingAnnotator
  override val distribution: Distribution[RNGRepr, Annotation] = DistributionAnnotator
  override val derived: Derived[RNGRepr, Annotation] = new DefaultDerived(this)
}
