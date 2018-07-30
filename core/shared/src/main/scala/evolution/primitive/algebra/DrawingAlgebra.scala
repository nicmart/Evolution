package evolution.primitive.algebra

import _root_.evolution.geometry.Point

trait BindingAlgebra[F[_]] {
  def var0[A]: F[A]
  def shift[A](expr: F[A]): F[A]
  def let[A, B](name: String, value: F[A])(expr: F[B]): F[B]
  def lambda[A, B](name: String, expr: F[B]): F[B]
  def fix[A](expr: F[A]): F[A]
}

trait CoreDrawingAlgebra[S[_], F[_]] {
  def empty[A]: F[A]
  def cons[A](head: S[A], tail: F[A]): F[A]
  def mapEmpty[A](eva: F[A])(eva2: F[A]): F[A]
  def mapCons[A, B](eva: F[A])(f: F[B]): F[B]
}

trait ScalarAlgebra[S[_]] {
  def double(d: Double): S[Double]
  def point(p: Point): S[Point]
}

trait DrawingAlgebra[S[_], F[_], R[_]] {
  type RS[T] = R[S[T]]
  type RF[T] = R[F[T]]
  val drawing: CoreDrawingAlgebra[RS, RF]
  val scalar: ScalarAlgebra[RS]
  val bind: BindingAlgebra[R]
}
