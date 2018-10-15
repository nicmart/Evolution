package evolution.primitive.algebra.evolution.interpreter

import cats.Id
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import evolution.algebra.EvolutionCoreAlgebra
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.{BindingEvaluator, EvaluationResult, Value}
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.constants.interpreter.ConstantsAlgebraEvaluator
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.list.interpreter.ListAlgebraEvaluator

// Generic TODO: Make sure we do as much as possible outside the closures
class EvolutionAlgebraEvaluator[F[+ _]](evolutionAlg: EvolutionCoreAlgebra[F])
    extends EvolutionAlgebra[Id, F, EvaluationResult, String] {
  override val list: ListAlgebra[Id, F, EvaluationResult] = new ListAlgebraEvaluator(evolutionAlg)
  override val constants: ConstantsAlgebra[EvaluationResult] = ConstantsAlgebraEvaluator
  override val bind: Binding[EvaluationResult, String] = BindingEvaluator
}
