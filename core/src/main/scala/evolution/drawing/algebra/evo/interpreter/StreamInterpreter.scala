package evolution.drawing.algebra.evo.interpreter

import StreamInterpreter.Repr
import evolution.drawing.algebra.evo.{Deterministic, EvoAlgebra, Full}

object StreamInterpreter {
  type Repr[W, A] = W => Stream[A]
}

class StreamInterpreter[W] extends EvoAlgebra[W, Repr[W, ?]] {
  override def constant[A](a: A): W => Stream[A] = _ => Stream.continually(a)
  override def autonomous[A](f: W => (W, Option[A])): W => Stream[A] =
    w => {
      val (w2, maybeA) = f(w)
      maybeA match {
        case None => Stream.empty
        case Some(a) => a #:: autonomous(f)(w2)
      }
    }
  override def state[A, S](s: S, f: S => Option[(S, A)]): W => Stream[A] = {
    def stream(from: S): Stream[A] = f(s) match {
      case None => Stream.empty
      case Some((s2, a)) => a #:: stream(s2)
    }
    _ => stream(s)
  }
  override def deterministic[A](f: Deterministic[A]): W => Stream[A] = {
    def stream(from: Deterministic[A]): Stream[A] = from.run() match {
      case None => Stream.empty
      case Some((a, next)) => a #:: stream(next)
    }
    _ => stream(f)
  }
  override def full[A](f: Full[W, A]): W => Stream[A] =
    w => {
      val (w2, maybeNext) = f.run(w)
      maybeNext match {
        case None => Stream.empty
        case Some((a, next)) => a #:: full(next)(w2)
      }
    }
  override def map[A, B](fa: W => Stream[A])(f: A => B): W => Stream[B] =
    w => fa(w).map(f)
  override def zipWith[A, B, C](fa: Repr[W, A], fb: Repr[W, B])(f: (A, B) => C): Repr[W, C] =
    w => fa(w).zip(fb(w)).map { case (a, b) => f(a, b)}
}