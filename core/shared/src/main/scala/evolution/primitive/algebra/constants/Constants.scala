package evolution.primitive.algebra.constants
import cats.kernel.{ Eq, Semigroup }
import cats.{ Group, ~> }
import cats.instances.function._
import evolution.geometry.Point
import evolution.typeclass.VectorSpace

trait Constants[R[_]] {
  def int(n: Int): R[Int]
  def double(d: Double): R[Double]
  def point(x: R[Double], y: R[Double]): R[Point]
  def x(point: R[Point]): R[Double]
  def y(point: R[Point]): R[Double]
  def add[T: Semigroup](a: R[T], b: R[T]): R[T]
  def div(a: R[Double], b: R[Double]): R[Double]
  def exp(a: R[Double], b: R[Double]): R[Double]
  def inverse[T: Group](a: R[T]): R[T]
  def multiply[T: VectorSpace](k: R[Double], t: R[T]): R[T]
  def sin(d: R[Double]): R[Double]
  def cos(d: R[Double]): R[Double]
  def eq[T: Eq](a: R[T], b: R[T]): R[Boolean]
  def ifThen[T](condition: R[Boolean], a: R[T], b: R[T]): R[T]
}
