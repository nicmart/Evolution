package evolution.drawing.algebra.initial

sealed trait EvoInitial[W, A]

final case class Constant[W, A](a: A) extends EvoInitial[W, A]
final case class Autonomous[W, A](f: W => (W, Option[A])) extends EvoInitial[W, A]
final case class State[W, A, S](start: S, f: S => Option[(S, A)]) extends EvoInitial[W, A]
final case class Deterministic[W, A](run: () => Option[(A, Deterministic[W, A])]) extends EvoInitial[W, A]
final case class Full[W, A](run: W => (W, Option[(A, Full[W, A])])) extends EvoInitial[W, A]
final case class Map[W, A, B](fa: EvoInitial[W, A], f: A => B) extends EvoInitial[W, B]

class MapOptimiser[W] {
  def apply[A](evoInitial: EvoInitial[W, A]): EvoInitial[W, A] =
    evoInitial match {
      case Map(Constant(a), f) => Constant[W, A](f(a))
      case Map(Map(fa, g), f) => Map(apply(fa), g andThen f)
      case _ => evoInitial
    }
}

class StreamInitialInterpreter[W] {
  def apply[A](evoInitial: EvoInitial[W, A]): W => Stream[A] = evoInitial match {
    case Constant(a) =>
      _ => Stream.continually(a)
    case Autonomous(f) => autonomous(f)
    case State(s, f) => state(s, f)
    case evo@Deterministic(_) => deterministic(evo)
    case evo@Full(_) => full(evo)
  }

  private def autonomous[A](f: W => (W, Option[A])): W => Stream[A] =
    w => {
      val (w2, maybeA) = f(w)
      maybeA match {
        case None => Stream.empty
        case Some(a) => a #:: autonomous(f)(w2)
      }
    }

  private def state[A, S](s: S, f: S => Option[(S, A)]): W => Stream[A] = {
    def stream(from: S): Stream[A] = f(s) match {
      case None => Stream.empty
      case Some((s2, a)) => a #:: stream(s2)
    }
    _ => stream(s)
  }

  def deterministic[A](f: Deterministic[W, A]): W => Stream[A] = {
    def stream(from: Deterministic[W, A]): Stream[A] = from.run() match {
      case None => Stream.empty
      case Some((a, next)) => a #:: stream(next)
    }
    _ => stream(f)
  }

  def full[A](f: Full[W, A]): W => Stream[A] =
    w => {
      val (w2, maybeNext) = f.run(w)
      maybeNext match {
        case None => Stream.empty
        case Some((a, next)) => a #:: full(next)(w2)
      }
    }

  def map[A, B](fa: W => Stream[A])(f: A => B): W => Stream[B] =
    w => fa(w).map(f)
}
