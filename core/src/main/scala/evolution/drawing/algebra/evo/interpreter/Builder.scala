package evolution.drawing.algebra.evo.interpreter

import evolution.drawing.algebra.evo.{Deterministic, EvoAlgebra, EvoExpr, Full}

class Builder[W] extends EvoAlgebra[W, EvoExpr[W, ?]] {
  override def constant[A](a: A): EvoExpr[W, A] = ???
  override def autonomous[A](f: W => (W, Option[A])): EvoExpr[W, A] = ???
  override def state[A, S](start: S, f: S => Option[(S, A)]): EvoExpr[W, A] = ???
  override def deterministic[A](f: Deterministic[A]): EvoExpr[W, A] = ???
  override def full[A](f: Full[W, A]): EvoExpr[W, A] = ???
  override def map[A, B](fa: EvoExpr[W, A])(f: A => B): EvoExpr[W, B] = ???
  override def zipWith[A, B, C](fa: EvoExpr[W, A], fb: EvoExpr[W, B])(f: (A, B) => C): EvoExpr[W, C] = ???
}