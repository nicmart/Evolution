package paint.evolution.algebra.syntax

trait NumericEvolutionSyntax {
  implicit final def numericSyntax[Evo[+ _], A](evo: Evo[A]): NumericEvolutionOps[Evo, A] =
    new NumericEvolutionOps(evo)
}

final class NumericEvolutionOps[Evo[+ _], A](val ev: Evo[A]) extends AnyVal {
}