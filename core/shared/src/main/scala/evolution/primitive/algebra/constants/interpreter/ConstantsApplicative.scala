package evolution.primitive.algebra.constants.interpreter
import cats.Applicative
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

class ConstantsApplicative[R1[_], R2[_]: Applicative](alg: Constants[R1]) extends Constants[Composed[R2, R1, ?]] {
  override def int(n: Int): R2[R1[Int]] =
    Applicative[R2].pure(alg.int(n))
  override def double(d: Double): R2[R1[Double]] =
    Applicative[R2].pure(alg.double(d))
  override def point(x: R2[R1[Double]], y: R2[R1[Double]]): R2[R1[Point]] =
    Applicative[R2].map2(x, y)(alg.point)
  override def add[T: VectorSpace](a: R2[R1[T]], b: R2[R1[T]]): R2[R1[T]] =
    Applicative[R2].map2(a, b)(alg.add[T])
  override def multiply[T: VectorSpace](k: Composed[R2, R1, Double], t: Composed[R2, R1, T]): Composed[R2, R1, T] =
    Applicative[R2].map2(k, t)(alg.multiply[T])
  override def sin(d: R2[R1[Double]]): R2[R1[Double]] =
    Applicative[R2].map(d)(alg.sin)
  override def cos(d: R2[R1[Double]]): R2[R1[Double]] =
    Applicative[R2].map(d)(alg.cos)
  override def eq[T: Eq](a: R2[R1[T]], b: R2[R1[T]]): R2[R1[Boolean]] =
    Applicative[R2].map2(a, b)(alg.eq[T])
  override def ifThen[T](
    condition: Composed[R2, R1, Boolean],
    a: Composed[R2, R1, T],
    b: Composed[R2, R1, T]): Composed[R2, R1, T] =
    Applicative[R2].map3(condition, a, b)(alg.ifThen[T])
}
