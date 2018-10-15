package evolution.primitive.algebra.evolution.interpreter

import cats.Id
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import evolution.algebra.EvolutionCoreAlgebra
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.{BindingEvaluator, EvaluationResult, Value}
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsEvaluator
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainEvaluator

// Generic TODO: Make sure we do as much as possible outside the closures
class EvolutionAlgebraEvaluator[F[+ _]](evolutionAlg: EvolutionCoreAlgebra[F])
    extends EvolutionAlgebra[Id, F, EvaluationResult, String] {
  override val list: Chain[Id, F, EvaluationResult] = new ChainEvaluator(evolutionAlg)
  override val constants: Constants[EvaluationResult] = ConstantsEvaluator
  override val bind: Binding[EvaluationResult, String] = BindingEvaluator
}
