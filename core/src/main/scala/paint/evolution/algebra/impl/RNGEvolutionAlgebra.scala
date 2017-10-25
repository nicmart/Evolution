package paint.evolution.algebra.impl

import paint.evolution.RNGEvolutionRepr
import paint.evolution.algebra.MaterializableFullAlgebra
import paint.random.RNG

import scala.collection.immutable.Stream

final class RNGEvolutionAlgebra extends MaterializableFullAlgebra[RNGEvolutionRepr, RNG] {

  override val empty: RNGEvolutionRepr[Nothing] =
    RNGEvolutionRepr { rng => (rng, None) }

  override def cons[A](head: A, tail: => RNGEvolutionRepr[A]): RNGEvolutionRepr[A] =
    RNGEvolutionRepr { rng => (rng, Some((head, tail))) }

  override def mapEmpty[A](eva: RNGEvolutionRepr[A])(eva2: => RNGEvolutionRepr[A]): RNGEvolutionRepr[A] = RNGEvolutionRepr[A] { rng =>
    val (rng2, next) = eva.run(rng)
    next match {
      case None => eva2.run(rng2)
      case _ => (rng2, next)
    }
  }

  override def mapCons[A, B](eva: RNGEvolutionRepr[A])
    (f: (A, RNGEvolutionRepr[A]) => RNGEvolutionRepr[B]): RNGEvolutionRepr[B] = RNGEvolutionRepr[B] { rng =>
    val (rng2, next) = eva.run(rng)
    next match {
      case None => (rng2, None)
      case Some((a, eva2)) => f(a, eva2).run(rng2)
    }
  }

  override def int: RNGEvolutionRepr[Int] = RNGEvolutionRepr { rng =>
    val (n, rng2) = rng.nextInt
    (rng2, Some((n, int)))
  }

  override def run[A](evo: RNGEvolutionRepr[A], rng1: RNG): Stream[A] = {
    val (rng2, next) = evo.run(rng1)
    next match {
      case None => Stream.empty
      case Some((a, eva2)) => a #:: run[A](eva2, rng2)
    }
  }
}
