package paint.evolution.algebra

import cats.data.NonEmptyList
import cats.kernel.{Group, Semigroup}
import cats.syntax.semigroup._
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import paint.evolution.algebra.syntax.all._

trait SemigroupEvolutionAlgebra[Evo[+_]] {
  self: EvolutionAlgebra[Evo] =>
  implicit val W: EvolutionAlgebra[Evo] = this

  def differentiate[A: Group](f: Evo[A]): Evo[A] =
    f.slidingPair.map{ case (a1, a2) => Group[A].remove(a1, a2) }

  def translate[A: Semigroup](ev1: Evo[A], ev2: Evo[A]): Evo[A] =
    ev1.zipWith(ev2)(Semigroup[A].combine)
}
