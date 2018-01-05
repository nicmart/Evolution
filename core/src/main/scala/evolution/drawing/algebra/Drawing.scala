package evolution.drawing.algebra

trait Drawing[-E, +A] {
  def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, A]
}
