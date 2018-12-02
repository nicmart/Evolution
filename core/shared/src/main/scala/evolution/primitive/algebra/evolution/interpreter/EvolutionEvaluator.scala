package evolution.primitive.algebra.evolution.interpreter

import cats.syntax.semigroup._
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator
import evolution.data.Result
import evolution.data.Result._
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainEvaluator
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsEvaluator
import evolution.primitive.algebra.derived.{ DefaultDerived, Derived }
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.distribution.interpreter.DistributionEvaluator
import evolution.primitive.algebra.evolution.Evolution

// Generic TODO: Make sure we do as much as possible outside the closures
object EvolutionEvaluator extends Evolution[RNGRepr, Result] {
  override val chain: Chain[RNGRepr, Result] = ChainEvaluator
  override val constants: Constants[Result] = ConstantsEvaluator
  override val bind: Binding[Result, String] = BindingEvaluator
  override val distribution: Distribution[RNGRepr, Result] = DistributionEvaluator
  override val derived: Derived[RNGRepr, Result] = new DefaultDerived(this)
}
