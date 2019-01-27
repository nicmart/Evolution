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
  def add[T: Semigroup](a: R[T], b: R[T]): R[T]
  def inverse[T: Group](a: R[T]): R[T]
  def multiply[T: VectorSpace](k: R[Double], t: R[T]): R[T]
  def sin(d: R[Double]): R[Double]
  def cos(d: R[Double]): R[Double]
  def eq[T: Eq](a: R[T], b: R[T]): R[Boolean]
  def ifThen[T](condition: R[Boolean], a: R[T], b: R[T]): R[T]
}

class MappedConstants[R1[_], R2[_]](alg: Constants[R1], to: R1 ~> R2, from: R2 ~> R1) extends Constants[R2] {
  override def int(n: Int): R2[Int] =
    to(alg.int(n))
  override def double(d: Double): R2[Double] =
    to(alg.double(d))
  override def point(x: R2[Double], y: R2[Double]): R2[Point] =
    to(alg.point(from(x), from(y)))
  override def add[T: Semigroup](a: R2[T], b: R2[T]): R2[T] =
    to(alg.add(from(a), from(b)))
  override def inverse[T: Group](a: R2[T]): R2[T] =
    to(alg.inverse(from(a)))
  override def sin(d: R2[Double]): R2[Double] =
    to(alg.sin(from(d)))
  override def cos(d: R2[Double]): R2[Double] =
    to(alg.cos(from(d)))
  override def multiply[T: VectorSpace](k: R2[Double], t: R2[T]): R2[T] =
    to(alg.multiply(from(k), from(t)))
  override def eq[T: Eq](a: R2[T], b: R2[T]): R2[Boolean] =
    to(alg.eq(from(a), from(b)))
  override def ifThen[T](condition: R2[Boolean], a: R2[T], b: R2[T]): R2[T] =
    to(alg.ifThen(from(condition), from(a), from(b)))
}
