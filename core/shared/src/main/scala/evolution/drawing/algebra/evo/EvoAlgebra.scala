package evolution.drawing.algebra.evo

trait EvoAlgebra[W, F[_]] {
  def constant[A](a: A): F[A]
  def autonomous[A](f: W => (W, Option[A])): F[A]
  def state[A, S](start: S, f: S => Option[(S, A)]): F[A]
  def deterministic[A](f: Deterministic[A]): F[A]
  def full[A](f: Full[W, A]): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]
  def zipWith[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
}

final case class Deterministic[A](run: () => Option[(A, Deterministic[A])])
final case class Full[W, A](run: W => (W, Option[(A, Full[W, A])]))

trait EvoExpr[W, A] {
  def run[F[_]](alg: EvoAlgebra[W, F]): F[A]
}