package paint.evolution.algebra.impl

import paint.evolution.{Evolution, NumericEvolutions}
import paint.evolution.algebra.MaterializableEvolutionAlgebra
import paint.random.RNG

final class RNGEvolutionAlgebra extends MaterializableEvolutionAlgebra[Evolution, RNG] {
  override def run[A](evo: Evolution[A], world: RNG): Stream[A] = evo.unfold(world)
  override def int: Evolution[Int] = NumericEvolutions.int
  override def empty[A]: Evolution[A] = Evolution.empty
  override def pure[A](a: A): Evolution[A] = Evolution.pure(a)
  override def flatMapNext[A, B](eva: Evolution[A])(f: (A, Evolution[A]) => Evolution[B]): Evolution[B] = eva.flatMapNext(f)
  override def concat[A](evo1: Evolution[A], evo2: =>Evolution[A]): Evolution[A] = evo1.append(evo2)
}
