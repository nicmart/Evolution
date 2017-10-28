package evolution.laws

import evolution.algebra.EvolutionAlgebra
import evolution.algebra.syntax.all._

trait EvolutionLaws[Evo[+ _]] {
  implicit val E: EvolutionAlgebra[Evo]

  import E._

  def pureLaw[A](a: A): IsEq[Evo[A]] =
    pure(a) <-> cons(a, empty)

  def covariantComposition[A, B, C](ev: Evo[A], f: A => B, g: B => C): IsEq[Evo[C]] =
    ev.map(f).map(g) <-> ev.map(f andThen g)

  def mapAsFlatmap[A, B](ev: Evo[A], f: A => B): IsEq[Evo[B]] =
    ev.map(f) <-> ev.flatMap(a => pure(f(a)))

  def mapConsLaw1[A, B](ev: Evo[A], a: A, f: (A, Evo[A]) => Evo[B]): IsEq[Evo[B]] =
    cons(a, ev).mapCons(f) <-> f(a, ev)

  def mapConsLaw2[A](ev: Evo[A]): IsEq[Evo[A]] =
    ev.mapCons { (a, ev2) => pure(a) } <-> ev.head

//  def scanLaw[A, Z](ev: Evo[A], f: (Z, A) => Z, z: Z, world: W): IsEq[Stream[Z]] =
//    ev.scan(z)(f).run(world) <-> ev.run(world).scanLeft(z)(f)

  //  def intIsAStaticEvolution(n: Int, m: Int): IsEq[Evolution[Int]] =
  //    staticEvolution(int, n, m)

  def repeatLaw[A](ev: Evo[A], n: Int): IsEq[Evo[A]] =
    ev.repeat(n) <-> concat(ev, ev.repeat(n - 1))

  def slowDownLaw[A](ev: Evo[A], a: A, n: Int): IsEq[Evo[A]] =
    cons(a, ev).slowDown(n) <-> concat(repeat(pure(a), n), ev.slowDown(n))

  def filterLaw[A](ev: Evo[A], a: A, predicate: A => Boolean): IsEq[Evo[A]] =
    cons(a, ev).filter(predicate) <-> (if (predicate(a)) cons(a, ev.filter(predicate)) else ev.filter(predicate))

  def slidingPairsLaw[A](ev: Evo[A], a1: A, a2: A): IsEq[Evo[(A, A)]] =
    cons(a1, cons(a2, ev)).slidingPair <-> cons((a1, a2), cons(a2, ev).slidingPair)

  def groupedLaw[A](ev: Evo[A], n: Int): IsEq[Evo[A]] =
    ev.grouped(n).flatMap(seq) <-> ev

  private def staticEvolution[A](ev: Evo[A], n: Int, m: Int): IsEq[Evo[A]] =
    concat(take(ev, n), take(ev, m)) <-> take(ev, n + m)
}
