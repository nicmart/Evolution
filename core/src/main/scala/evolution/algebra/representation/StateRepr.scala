package evolution.algebra.representation

import scala.collection.immutable.Stream

trait StateRepr[+A, S, +Self <: StateRepr[A, S, Self]] {
  def run: S => (S, Option[(A, Self)])
  def unfold(rng1: S): Stream[A] = {
    val (rng2, next) = run(rng1)
    next match {
      case None => Stream.empty
      case Some((a, eva2)) => {
        val ss: StateRepr[A, S, Self] = eva2
        a #:: eva2.unfold(rng2)
      }
    }
  }
}