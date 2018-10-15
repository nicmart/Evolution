package evolution.laws

import evolution.algebra.LegacyEvolutionAlgebra
import evolution.algebra.syntax.all._

trait EvolutionLaws[Evo[+ _]] {
  implicit val E: LegacyEvolutionAlgebra[Evo]

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
    ev.mapCons { (a, ev2) =>
      pure(a)
    } <-> ev.head

  def consLaw[A](ev: Evo[A], a: A): IsEq[Evo[A]] = {
    def rec: Evo[A] = cons(a, rec)
    rec <-> rec
  }

  def concatIsStackSafeLaw[A](a: A): IsEq[Evo[A]] = {
    var evo: Evo[A] = empty
    (1 to 5000).foreach { _ =>
      evo = concat(pure(a), empty)
    }
    evo <-> evo
  }

  def repeatLaw[A](ev: Evo[A], n: Int): IsEq[Evo[A]] =
    ev.repeat(n) <-> ev.repeat(n)

  def slowDownLaw[A](ev: Evo[A], a: A, n: Int): IsEq[Evo[A]] =
    cons(a, ev).slowDown(n) <-> concat(repeat(pure(a), n), ev.slowDown(n))

  def filterLaw[A](ev: Evo[A], a: A, predicate: A => Boolean): IsEq[Evo[A]] =
    cons(a, ev).filter(predicate) <-> (if (predicate(a)) cons(a, ev.filter(predicate)) else ev.filter(predicate))

  def slidingPairsLaw[A](ev: Evo[A], a1: A, a2: A): IsEq[Evo[(A, A)]] =
    cons(a1, cons(a2, ev)).slidingPair <-> cons((a1, a2), cons(a2, ev).slidingPair)
}
