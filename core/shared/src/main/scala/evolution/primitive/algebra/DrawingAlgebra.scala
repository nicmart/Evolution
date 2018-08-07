package evolution.primitive.algebra

import _root_.evolution.geometry.Point
import cats.kernel.Semigroup
import cats.syntax.semigroup._

trait BindingAlgebra[R[_]] {
  def var0[A]: R[A]
  def shift[A](expr: R[A]): R[A]
  def let[A, B](name: String, value: R[A])(expr: R[B]): R[B]
  def lambda[A, B](name: String, expr: R[B]): R[A => B]
  def app[A, B](f: R[A => B], a: R[A]): R[B]
  def fix[A](expr: R[A]): R[A]
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
  val drawing: CoreDrawingAlgebra[S, F, R]
  val scalar: ScalarAlgebra[RS]
  val bind: BindingAlgebra[R]
}
