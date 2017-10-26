package paint.evolution

import paint.random.RNG

import scala.collection.immutable.Stream

final case class Repr[+A](run: RNG => (RNG, Option[(A, Repr[A])])) {
  def unfold(rng1: RNG): Stream[A] = {
    val (rng2, next) = run(rng1)
    next match {
      case None => Stream.empty
      case Some((a, eva2)) => a #:: eva2.unfold(rng2)
    }
  }
}

//final class ReprF[+A, F[+_]](val run: RNG => (RNG, Option[(A, F[ReprF[A, F]])])) extends AnyVal
//trait ReprAbstract {
//  type Id[+A]
//  type Repr[+A] = ReprF[A, Id]
//}
//
//object Repr extends ReprAbstract{
//  type Id[+A] = A
//  def apply[A](run: RNG => (RNG, Option[(A, Repr[A])])): Repr[A] =
//    new ReprF[A, Id](run)
//}

