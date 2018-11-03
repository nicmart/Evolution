package evolution.primitive.algebra.evolution.interpreter

import cats.Id
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.{BindingEvaluator, EvaluationResult}
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainEvaluator
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsEvaluator
import evolution.primitive.algebra.evolution.Evolution

// Generic TODO: Make sure we do as much as possible outside the closures
object EvolutionEvaluator extends Evolution[RNGRepr, EvaluationResult, Double, String, String] {
  override val list: Chain[RNGRepr, EvaluationResult] = ChainEvaluator
  override val constants: Constants[EvaluationResult, Double] = ConstantsEvaluator
  override val bind: Binding[EvaluationResult, String, String] = BindingEvaluator
}
