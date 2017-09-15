package paint.evolution.`new`.impl

import paint.evolution.{Evolution, NumericEvolutions}
import paint.evolution.`new`.Evolutions
import paint.random.RNG

final class RNGEvolutions extends Evolutions[Evolution, RNG] {
  override def run[A](evo: Evolution[A], world: RNG): Stream[A] = evo.unfold(world)
  override def extractInt(world: RNG): Int = world.nextInt._1
  override def int: Evolution[Int] = NumericEvolutions.int
  override def worlds: Evolution[RNG] = Evolution { rng =>
    val (n, rng2) = rng.nextInt
    (rng2, Some((rng2, worlds)))
  }
  override def empty[A]: Evolution[A] = Evolution.empty
  override def pure[A](a: A): Evolution[A] = Evolution.pure(a)
  override def flatMap[A, B](eva: Evolution[A])(f: (A) => Evolution[B]): Evolution[B] = eva.flatMap(f)
  override def concat[A](evo1: Evolution[A], evo2: =>Evolution[A]): Evolution[A] = evo1.append(evo2)
  override def take[A](evo: Evolution[A], n: Int): Evolution[A] = evo.take(n)
}
