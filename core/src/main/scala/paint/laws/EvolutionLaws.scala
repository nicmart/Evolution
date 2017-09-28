package paint.laws

import paint.evolution.algebra.MaterializableEvolutionAlgebra
import paint.evolution.algebra.syntax.all._

trait EvolutionLaws[Evolution[+_], W] {
  implicit val E: MaterializableEvolutionAlgebra[Evolution, W]
  import E._

  def pureLaw[A](a: A, world: W): IsEq[Stream[A]] =
    pure(a).run(world) <-> Stream(a)

  def covariantComposition[A, B, C](ev: Evolution[A], f: A => B, g: B => C): IsEq[Evolution[C]] =
    ev.map(f).map(g) <-> ev.map(f andThen g)

  def mapAsFlatmap[A, B](ev: Evolution[A], f: A => B): IsEq[Evolution[B]] =
    ev.map(f) <-> ev.flatMap(a => pure(f(a)))

  def scanLaw[A, Z](ev: Evolution[A], f: (Z, A) => Z, z: Z, world: W): IsEq[Stream[Z]] =
    ev.scan(z)(f).run(world) <-> ev.run(world).scanLeft(z)(f)

  def intIsAStaticEvolution(n: Int): IsEq[Evolution[Int]] =
    staticEvolution(int, n)

  def repeatLaw[A](ev: Evolution[A], n: Int): IsEq[Evolution[A]] =
    ev.repeat(n) <-> concat(ev, ev.repeat(n - 1))

  def slowDownLaw[A](ev: Evolution[A], n: Int, world: W): IsEq[Stream[A]] =
    ev.slowDown(n).run(world) <-> ev.run(world).flatMap { a => Stream.fill(n)(a) }

  def filterLaw[A](ev: Evolution[A], predicate: A => Boolean, world: W): IsEq[Stream[A]] =
    ev.run(world).filter(predicate) <-> ev.filter(predicate).run(world)

  def slidingPairsLaw[A](ev: Evolution[A], world: W): IsEq[Stream[(A, A)]] =
    ev.slidingPair.run(world) <-> ev.run(world).sliding(2).filter(_.length == 2).toStream.map(as => (as(0), as(1)))

  private def staticEvolution[A](ev: Evolution[A], n: Int): IsEq[Evolution[A]] =
    ev <-> ev.drop(n)
}
