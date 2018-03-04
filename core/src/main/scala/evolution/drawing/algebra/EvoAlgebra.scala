package evolution.drawing.algebra

import evolution.drawing.algebra.Optimiser.MapOptimiser
import evolution.drawing.algebra.StreamInterpreter.Repr

trait EvoAlgebra[W, F[_]] {
  def constant[A](a: A): F[A]
  def autonomous[A](f: W => (W, Option[A])): F[A]
  def state[A, S](start: S, f: S => Option[(S, A)]): F[A]
  def deterministic[A](f: Deterministic[A]): F[A]
  def full[A](f: Full[W, A]): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

final case class Deterministic[A](run: () => Option[(A, Deterministic[A])])
final case class Full[W, A](run: W => (W, Option[(A, Full[W, A])]))

trait EvoExpr[A, W] {
  def run[F[_]](alg: EvoAlgebra[W, F]): F[A]
}

class Builder[W] extends EvoAlgebra[W, EvoExpr[?, W]] {
  override def constant[A](a: A): EvoExpr[A, W] = ???
  override def autonomous[A](f: W => (W, Option[A])): EvoExpr[A, W] = ???
  override def state[A, S](start: S, f: S => Option[(S, A)]): EvoExpr[A, W] = ???
  override def deterministic[A](f: Deterministic[A]): EvoExpr[A, W] = ???
  override def full[A](f: Full[W, A]): EvoExpr[A, W] = ???
  override def map[A, B](fa: EvoExpr[A, W])(f: A => B): EvoExpr[B, W] = ???
}

class Optimiser[W] extends EvoAlgebra[W, EvoExpr[?, W]] {
  private val b = new Builder[W]
  override def constant[A](a: A): EvoExpr[A, W] = b.constant(a)
  override def autonomous[A](f: W => (W, Option[A])): EvoExpr[A, W] = b.autonomous(f)
  override def state[A, S](start: S, f: S => Option[(S, A)]): EvoExpr[A, W] = b.state(start, f)
  override def deterministic[A](f: Deterministic[A]): EvoExpr[A, W] = b.deterministic(f)
  override def full[A](f: Full[W, A]): EvoExpr[A, W] = b.full(f)
  override def map[A, B](fa: EvoExpr[A, W])(f: A => B): EvoExpr[B, W] =
    fa.run[Optimiser.Repr[?, B, W]](new Optimiser.MapOptimiser)(f)
}

object Optimiser {
  type Repr[A, B, W] = (A => B) => EvoExpr[B, W]
  class MapOptimiser[B, W] extends EvoAlgebra[W, Repr[?, B, W]] {
    val builder = new Builder[W]
    // Here is the optimisation
    override def constant[A](a: A): Repr[A, B, W] =
      fab => builder.constant(fab(a))
    // Collapse maps
    override def map[A, C](fa: Repr[A, B, W])(f: A => C): Repr[C, B, W] =
      fcb => fa(f andThen fcb)
    // Just map in the other ones
    override def autonomous[A](f: W => (W, Option[A])): Repr[A, B, W] =
      fab => builder.map(builder.autonomous(f))(fab)
    override def state[A, S](start: S, f: S => Option[(S, A)]): Repr[A, B, W] =
      fab => builder.map(builder.state(start, f))(fab)
    override def deterministic[A](f: Deterministic[A]): Repr[A, B, W] =
      fab => builder.map(builder.deterministic(f))(fab)
    override def full[A](f: Full[W, A]): Repr[A, B, W] =
      fab => builder.map(builder.full(f))(fab)
  }
}

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
}