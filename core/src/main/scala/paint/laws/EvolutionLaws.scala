package paint.laws

import paint.evolution.algebra.MaterializableEvolutionAlgebra
import paint.evolution.algebra.syntax.all._

trait EvolutionLaws[Evolution[_], W] {
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

  def intIsAStaticEvolution(n: Int, m: Int): IsEq[Evolution[Int]] =
    staticEvolution(int, n, m)

  private def staticEvolution[A](ev: Evolution[A], n: Int, m: Int): IsEq[Evolution[A]] =
    concat(ev.take(n), ev.take(m)) <-> ev.take(n + m)
}
