package paint.evolution.algebra.syntax

import paint.evolution.algebra.{EvolutionAlgebra, EvolutionCoreAlgebra, EvolutionMaterialization}

trait NumericEvolutionSyntax {
  implicit final def syntaxEvolution[Evo[+_], A](evo: Evo[A]): NumericEvolutionOps[Evo, A] =
    new NumericEvolutionOps(evo)
}

final class NumericEvolutionOps[Evo[+_], A](val ev: Evo[A]) extends AnyVal {
}