package paint.evolution.algebra

import cats.kernel.{Group, Semigroup}
import paint.evolution.algebra.syntax.all._

trait SemigroupEvolutionAlgebra[Evo[+ _]] extends EvolutionAlgebra[Evo] {
  private implicit lazy val E: EvolutionAlgebra[Evo] = this

  def differentiate[A: Group](f: Evo[A]): Evo[A] =
    f.slidingPair.map { case (a1, a2) => Group[A].remove(a1, a2) }

  def translate[A: Semigroup](ev1: Evo[A], ev2: Evo[A]): Evo[A] =
    ev1.zipWith(ev2)(Semigroup[A].combine)

  def centredIn[A](center: A)(evo: Evo[A])(implicit semigroup: Semigroup[A]): Evo[A] =
    evo.map(semigroup.combine(center, _))
}
