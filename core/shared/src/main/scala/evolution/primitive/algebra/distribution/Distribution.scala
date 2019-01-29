package evolution.primitive.algebra.distribution

trait Distribution[F[_], R[_]] {
  def uniform(from: R[Double], to: R[Double]): R[F[Double]]
  def uniformChoice[T](ts: List[R[T]]): R[F[T]]
}
