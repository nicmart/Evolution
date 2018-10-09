package evolution.primitive.algebra.constants.generator
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.{Composed, Generator}
import evolution.primitive.algebra.constants.ConstantsAlgebra
import org.scalacheck.Gen

class ConstantsAlgebraGenerator[S[_]](alg: ConstantsAlgebra[S]) extends ConstantsAlgebra[Generator[S, ?]] {

  override def double(d: Double): Generator[S, Double] =
    _ => Gen.const(alg.double(d))

  override def point(genX: Generator[S, Double], genY: Generator[S, Double]): Generator[S, Point] =
    n =>
      for {
        x <- genX(n)
        y <- genY(n)
      } yield alg.point(x, y)

  override def add[T: Semigroup](genA: Generator[S, T], genB: Generator[S, T]): Generator[S, T] =
    n =>
      for {
        a <- genA(n)
        b <- genB(n)
      } yield alg.add(a, b)
}
