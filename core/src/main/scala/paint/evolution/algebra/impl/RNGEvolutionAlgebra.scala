package paint.evolution.algebra.impl

import paint.evolution.{Evolution, NumericEvolutions}
import paint.evolution.algebra.MaterializableEvolutionAlgebra
import paint.random.RNG

final class RNGEvolutionAlgebra extends MaterializableEvolutionAlgebra[Evolution, RNG] {
  override def run[A](evo: Evolution[A], world: RNG): Stream[A] = evo.unfold(world)
  override def int: Evolution[Int] = NumericEvolutions.int
  override def empty[A]: Evolution[A] = Evolution.empty
  override def pure[A](a: A): Evolution[A] = Evolution.pure(a)
  override def flatMap[A, B](eva: Evolution[A])(f: (A) => Evolution[B]): Evolution[B] = eva.flatMap(f)
  override def concat[A](evo1: Evolution[A], evo2: =>Evolution[A]): Evolution[A] = evo1.append(evo2)
  override def take[A](evo: Evolution[A], n: Int): Evolution[A] = evo.take(n)
  override def drop[A](evo: Evolution[A], n: Int): Evolution[A] = evo.drop(n)
  override def scan[Z, A](evo: Evolution[A])(z: Z)(f: (Z, A) => Z): Evolution[Z] =
    concat(pure(z), evo.flatMapNext { (a, evo2) => scan(evo2)(f(z, a))(f)})
  override def zipWith[A, B, C](eva: Evolution[A], evb: Evolution[B])(f: (A, B) => C): Evolution[C] =
    eva.zipWith(evb)(f)
}
