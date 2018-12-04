package evolution.algebra.representation

import scala.collection.immutable.Stream

trait StateRepr[+A, S, +Self <: StateRepr[A, S, Self]] {
  def run: S => (S, Option[(A, Self)])
  def unfold(rng1: S): Stream[A] = {
    val (rng2, next) = run(rng1)
    next match {
      case None => Stream.empty
      case Some((a, eva2)) => {
        a #:: eva2.unfold(rng2)
      }
    }
  }

  def iterator(rng1: S): Iterator[A] = new Iterator[A] {
    val firstRun = run(rng1)
    var currentState = firstRun._1
    var maybeNext: Option[(A, Self)] = firstRun._2

    override def hasNext: Boolean = maybeNext.isDefined

    override def next(): A = {
      maybeNext match {
        case Some((a, self)) =>
          val nextRun = self.run(currentState)
          currentState = nextRun._1
          maybeNext = nextRun._2
          a
      }
    }
  }
}
