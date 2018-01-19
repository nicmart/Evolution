package evolution.drawing.algebra

import cats.kernel.{Group, Semigroup}
import cats.implicits._
import evolution.geometry.Point

trait BindingAlgebra[F[-_, +_]] {
  def var0[E, A]: F[(F[E, A], E), A]
  def shift[E, A, B](expr: F[E, A]): F[(F[E, B], E), A]
  def let[E, A, B](name: String, value: F[E, A])(expr: F[(F[E, A], E), B]): F[E, B]
}

trait DrawingAlgebra[F[-_, +_]] extends BindingAlgebra[F] {
  def const[E, T: DrawingAlgebra.Type](x: T): F[E, T]
  def rnd[E](from: Double, to: Double): F[E, Double]
  def point[E](x: F[E, Double], y: F[E, Double]): F[E, Point]
  def polar[E](r: F[E, Double], w: F[E, Double]): F[E, Point]
  def integrate[E, T : DrawingAlgebra.Type](start: T, f: F[E, T]): F[E, T]
  def derive[E, T : DrawingAlgebra.Type](f: F[E, T]): F[E, T]
}

object DrawingAlgebra {
  sealed trait Type[T] {
    def group: Group[T]
    def fold[B](t: T)(f1: Double => B, f2: Point => B): B
    def foldT[F[_]](ifDouble: => F[Double], ifPoint: => F[Point]): F[T]
  }
  implicit final object DoubleType extends Type[Double] {
    override def group: Group[Double] = Group[Double]
    override def fold[B](t: Double)(f1: Double => B, f2: Point => B): B = f1(t)
    override def foldT[F[_]](ifDouble: => F[Double], ifPoint: => F[Point]): F[Double] = ifDouble
  }
  implicit final object PointType extends Type[Point] {
    override def group: Group[Point] = Group[Point]
    override def fold[B](t: Point)(f1: Double => B, f2: Point => B): B = f2(t)
     override def foldT[F[_]](ifDouble: => F[Double], ifPoint: => F[Point]): F[Point] = ifPoint
  }
  def typeInstance[T](implicit t: Type[T]): Type[T] = t
}
