package evolution.drawing.algebra

trait DrawingE[E[_[_, _]], +A] {
  def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], A]
}
