package paint.evolution

import cats.kernel.{Group, Semigroup}

object SemigroupEvolutions {
  def differentiate[A: Group](f: Evolution[A]): Evolution[A] =
    f.slidingPairs.map { case (a1, a2) => Group[A].remove(a1, a2) }

  def translate[A: Semigroup](ev1: Evolution[A], ev2: Evolution[A]): Evolution[A] =
    ev1.zipWith(ev2)(Semigroup[A].combine)
}
