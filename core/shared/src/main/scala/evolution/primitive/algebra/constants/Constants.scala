package evolution.primitive.algebra.constants
import cats.kernel.Semigroup
import cats.~>
import cats.instances.function._
import evolution.geometry.Point
import evolution.primitive.algebra.constants.interpreter.ConstantsApplicative
import evolution.typeclass.VectorSpace

trait Constants[R[_], D] {
  def double(d: D): R[Double]
  def point(x: R[Double], y: R[Double]): R[Point]
  def add[T: VectorSpace](a: R[T], b: R[T]): R[T]
  def sin(d: R[Double]): R[Double]
  def cos(d: R[Double]): R[Double]
}

class MappedConstants[R1[_], R2[_], D](alg: Constants[R1, D], to: R1 ~> R2, from: R2 ~> R1) extends Constants[R2, D] {
  def double(d: D): R2[Double] =
    to(alg.double(d))
  def point(x: R2[Double], y: R2[Double]): R2[Point] =
    to(alg.point(from(x), from(y)))
  def add[T: VectorSpace](a: R2[T], b: R2[T]): R2[T] =
    to(alg.add(from(a), from(b)))
  override def sin(d: R2[Double]): R2[Double] =
    to(alg.sin(from(d)))
  override def cos(d: R2[Double]): R2[Double] =
    to(alg.cos(from(d)))
}
