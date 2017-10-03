package paint.evolution.algebra.syntax

import cats.kernel.Group
import paint.evolution.algebra.{EvolutionAlgebra, EvolutionCoreAlgebra, EvolutionMaterialization, SemigroupEvolutionAlgebra}

trait SemigroupEvolutionSyntax {
  implicit final def semigroupSyntax[Evo[+_], A](evo: Evo[A]): SemigroupEvolutionOps[Evo, A] =
    new SemigroupEvolutionOps(evo)
}

final class SemigroupEvolutionOps[Evo[+_], A](val evo: Evo[A]) extends AnyVal {
  def differentiate(implicit E: SemigroupEvolutionAlgebra[Evo], group: Group[A]): Evo[A] =
    E.differentiate(evo)
  def translate(other: Evo[A])(implicit E: SemigroupEvolutionAlgebra[Evo], group: Group[A]): Evo[A] =
    E.translate(evo, other)
}