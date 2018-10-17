package evolution.primitive.algebra.evolution.interpreter

import cats.Id
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import evolution.algebra.LegacyEvolutionCoreAlgebra
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.{BindingEvaluator, EvaluationResult, Value}
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsEvaluator
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainEvaluator

// Generic TODO: Make sure we do as much as possible outside the closures
class EvolutionEvaluator[F[+ _]](evolutionAlg: LegacyEvolutionCoreAlgebra[F])
    extends Evolution[Id, F, EvaluationResult, Double, String, String] {
  override val list: Chain[Id, F, EvaluationResult] = new ChainEvaluator(evolutionAlg)
  override val constants: Constants[EvaluationResult, Double] = ConstantsEvaluator
  override val bind: Binding[EvaluationResult, String, String] = BindingEvaluator
}
