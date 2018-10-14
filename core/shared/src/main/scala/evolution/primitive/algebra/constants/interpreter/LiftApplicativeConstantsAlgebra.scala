package evolution.primitive.algebra.constants.interpreter
import cats.Applicative
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.constants.ConstantsAlgebra

class LiftApplicativeConstantsAlgebra[S[_], F[_]: Applicative](alg: ConstantsAlgebra[S])
    extends ConstantsAlgebra[Composed[F, S, ?]] {
  override def double(d: Double): F[S[Double]] =
    Applicative[F].pure(alg.double(d))
  override def point(x: F[S[Double]], y: F[S[Double]]): F[S[Point]] =
    Applicative[F].map2(x, y)(alg.point)
  override def add[T: Semigroup](a: F[S[T]], b: F[S[T]]): F[S[T]] =
    Applicative[F].map2(a, b)(alg.add[T])
}
