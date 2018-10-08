package evolution.primitive.algebra.constants.generator
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.constants.ConstantsAlgebra
import org.scalacheck.Gen

class ConstantsAlgebraGenerator[S[_]](alg: ConstantsAlgebra[S]) extends ConstantsAlgebra[Composed[Gen, S, ?]] {

  override def double(d: Double): Gen[S[Double]] =
    Gen.const(alg.double(d))

  override def point(genX: Gen[S[Double]], genY: Gen[S[Double]]): Gen[S[Point]] =
    for {
      x <- genX
      y <- genY
    } yield alg.point(x, y)

  override def add[T: Semigroup](genA: Gen[S[T]], genB: Gen[S[T]]): Gen[S[T]] =
    for {
      a <- genA
      b <- genB
    } yield alg.add(a, b)
}
