package evolution.primitive.algebra.derived
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution

trait Derived[F[_], R[_]] {
  def cartesian(x: R[F[Double]], y: R[F[Double]]): R[F[Point]]
}

class DefaultDerived[F[_], R[_]](alg: Evolution[F, R, Double, String, String]) extends Derived[F, R] {
  import alg.bind._, alg.chain._, alg.constants._, alg.distribution._

  def flatMap[A, B](fa: R[F[A]], f: R[A => F[B]]): R[F[B]] =
    ???

  def map[A, B](fa: R[F[A]], f: R[A => B]): R[F[B]] =
    ???

  def map2[A, B, C](fa: R[F[A]], fb: R[F[B]], f: R[A => B => C]): R[F[C]] =
    ???

  override def cartesian(x: R[F[Double]], y: R[F[Double]]): R[F[Point]] =
    map2(x, y, lambda("x", lambda("y", point(shift(var0[Double]), var0[Double]))))
}
