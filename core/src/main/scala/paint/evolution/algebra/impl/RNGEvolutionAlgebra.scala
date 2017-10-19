package paint.evolution.algebra.impl

import paint.evolution.Evolution
import paint.evolution.algebra.MaterializableFullAlgebra
import paint.random.RNG

final class RNGEvolutionAlgebra extends MaterializableFullAlgebra[Evolution, RNG] {
  override def run[A](evo: Evolution[A], world: RNG): Stream[A] =
    evo.unfold(world)
  override val empty: Evolution[Nothing] =
    Evolution.empty
  override def flatMapNext[A, B](eva: Evolution[A])(f: (A, Evolution[A]) => Evolution[B]): Evolution[B] =
    eva.flatMapNext(f)
  override def cons[A](head: A, tail: => Evolution[A]): Evolution[A] =
    Evolution.pure(head).append(tail)
  override def flatMapEmpty[A](eva: Evolution[A])(eva2: => Evolution[A]): Evolution[A] =
    Evolution { rng =>
      val (rng2, next) = eva.run(rng)
      next match {
        case None => eva2.run(rng2)
        case _ => (rng2, next)
      }
    }
  override def int: Evolution[Int] = Evolution { rng =>
    val (n, rng2) = rng.nextInt
    (rng2, Some((n, int)))
  }
}
