package paint.evolution

import paint.random.RNG

final class RNGEvolutionRepr[+A](val run: RNG => (RNG, Option[(A, RNGEvolutionRepr[A])]))

object RNGEvolutionRepr {
  def apply[A](run: RNG => (RNG, Option[(A, RNGEvolutionRepr[A])])): RNGEvolutionRepr[A] =
    new RNGEvolutionRepr[A](run)
}