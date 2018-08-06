package evolution.primitive.algebra

import _root_.evolution.geometry.Point
import cats.kernel.Semigroup
import cats.syntax.semigroup._

trait BindingAlgebra[F[_]] {
  def var0[A]: F[A]
  def shift[A](expr: F[A]): F[A]
  def let[A, B](name: String, value: F[A])(expr: F[B]): F[B]
  def lambda[A, B](name: String, expr: F[B]): F[A => B]
  def app[A, B](f: F[A => B], a: F[A]): F[B]
  def fix[A](expr: F[A => A]): F[A]
}

trait CoreDrawingAlgebra[S[_], F[_], R[_]] {
  def empty[A]: R[F[A]]
  def cons[A](head: R[S[A]], tail: R[F[A]]): R[F[A]]
  def mapEmpty[A](eva: R[F[A]])(eva2: R[F[A]]): R[F[A]]
  def mapCons[A, B](eva: R[F[A]])(f: R[S[A] => F[A] => F[B]]): R[F[B]]
}

trait ScalarAlgebra[S[_]] {
  def double(d: Double): S[Double]
  def point(x: Double, y: Double): S[Point]
  def add[T: Semigroup](a: S[T], b: S[T]): S[T]
}

trait DrawingAlgebra[S[_], F[_], R[_]] {
  type RS[T] = R[S[T]]
  type RF[T] = R[F[T]]
  val drawing: CoreDrawingAlgebra[RS, RF]
  val scalar: ScalarAlgebra[RS]
  val bind: BindingAlgebra[R]
}
