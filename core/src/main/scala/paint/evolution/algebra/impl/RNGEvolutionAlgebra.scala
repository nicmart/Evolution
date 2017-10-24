package paint.evolution.algebra.impl

import paint.evolution.EvolutionLegacy
import paint.evolution.algebra.MaterializableFullAlgebra
import paint.random.RNG

final class RNGEvolutionAlgebra extends MaterializableFullAlgebra[EvolutionLegacy, RNG] {
  override def run[A](evo: EvolutionLegacy[A], world: RNG): Stream[A] =
    evo.unfold(world)
  override val empty: EvolutionLegacy[Nothing] =
    EvolutionLegacy.empty
  override def mapCons[A, B](eva: EvolutionLegacy[A])(f: (A, EvolutionLegacy[A]) => EvolutionLegacy[B]): EvolutionLegacy[B] =
    eva.flatMapNext(f)
  override def cons[A](head: A, tail: => EvolutionLegacy[A]): EvolutionLegacy[A] =
    EvolutionLegacy.pure(head).append(tail)
  override def mapEmpty[A](eva: EvolutionLegacy[A])(eva2: => EvolutionLegacy[A]): EvolutionLegacy[A] =
    EvolutionLegacy { rng =>
      val (rng2, next) = eva.run(rng)
      next match {
        case None => eva2.run(rng2)
        case _ => (rng2, next)
      }
    }
  override def int: EvolutionLegacy[Int] = EvolutionLegacy { rng =>
    val (n, rng2) = rng.nextInt
    (rng2, Some((n, int)))
  }
}
