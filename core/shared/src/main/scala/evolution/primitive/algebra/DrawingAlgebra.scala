package evolution.primitive.algebra

import _root_.evolution.geometry.Point

trait BindingAlgebra[F[_]] {
  def var0[A]: F[A]
  def shift[A](expr: F[A]): F[A]
  def let[A, B](name: String, value: F[A])(expr: F[B]): F[B]
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

trait DrawingAlgebra[S[_], F[_]] {
  val drawing: CoreDrawingAlgebra[S, F]
  val scalar: ScalarAlgebra[S]
  val bindS: BindingAlgebra[S]
  val bindF: BindingAlgebra[F]
}

trait DrawingExpr[A] {
  def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A]
}

trait ScalarExpr[A] {
  def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): S[A]
}
