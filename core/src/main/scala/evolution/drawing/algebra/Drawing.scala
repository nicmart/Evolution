package evolution.drawing.algebra

trait Drawing[+A] {
  def run[F[+_]](alg: DrawingAlgebra[F]): F[A]
}
