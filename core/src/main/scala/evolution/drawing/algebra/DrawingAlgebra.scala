package evolution.drawing.algebra

import cats.kernel.{Group, Semigroup}
import cats.implicits._
import evolution.geometry.Point

trait DrawingAlgebra[F[+_]] {
  def const[T: DrawingAlgebra.Type](x: T): F[T]
  def rnd(from: Double, to: Double): F[Double]
  def cartesian(x: F[Double], y: F[Double]): F[Point]
  def polar(r: F[Double], w: F[Double]): F[Point]
  def integrate[T : DrawingAlgebra.Type](start: T, f: F[T]): F[T]
  def derive[T : DrawingAlgebra.Type](f: F[T]): F[T]
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
