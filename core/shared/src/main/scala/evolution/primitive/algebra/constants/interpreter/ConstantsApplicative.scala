package evolution.primitive.algebra.constants.interpreter
import cats.Applicative
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

class ConstantsApplicative[R1[_], D, R2[_]: Applicative](alg: Constants[R1, D])
    extends Constants[Composed[R2, R1, ?], D] {
  override def double(d: D): R2[R1[Double]] =
    Applicative[R2].pure(alg.double(d))
  override def point(x: R2[R1[Double]], y: R2[R1[Double]]): R2[R1[Point]] =
    Applicative[R2].map2(x, y)(alg.point)
  override def add[T: VectorSpace](a: R2[R1[T]], b: R2[R1[T]]): R2[R1[T]] =
    Applicative[R2].map2(a, b)(alg.add[T])
  override def sin(d: R2[R1[Double]]): R2[R1[Double]] =
    Applicative[R2].map(d)(alg.sin)
  override def cos(d: R2[R1[Double]]): R2[R1[Double]] =
    Applicative[R2].map(d)(alg.cos)
}
