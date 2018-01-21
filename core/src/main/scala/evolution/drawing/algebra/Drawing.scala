package evolution.drawing.algebra

import scala.Predef.=:=

trait Drawing[-E, +A] {
  def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, A]
}

trait DrawingOpen[E, A, B] {
  def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[(F[E, A], E), B]
}

trait DrawingG[+A, E[_[_, _]]] {
  def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], A]
}

object DrawingG {
  type Empty[F[_, _]] = Unit
  type ConstE[E, F[_, _]] = E
  type SuccE[A, E[_[_, _]], F[_, _]] = (F[A, E[F]], E[F])
  type DrawingZ[A, E] = DrawingG[A, Lambda[F[_, _] => E]]
  type DrawingS[A, B, E[_[_, _]]] = DrawingG[A, Lambda[F[_, _] => SuccE[B, E, F]]]
  def shift[A, E[_[_, _]]](drawing: DrawingG[A, E]): DrawingS[A, A, E] = ???
}
