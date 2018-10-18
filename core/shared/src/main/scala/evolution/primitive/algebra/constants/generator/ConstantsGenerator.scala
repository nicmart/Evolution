package evolution.primitive.algebra.constants.generator
import cats.kernel.Semigroup
import evolution.generator.Generator
import evolution.geometry.Point
import evolution.primitive.algebra.{Composed, GenRepr}
import evolution.primitive.algebra.constants.Constants
import org.scalacheck.Arbitrary.arbitrary

// TODO this can be a an applicative lifted algebra, with an overriden double method
class ConstantsGenerator[S[_]](alg: Constants[S, Double]) extends Constants[GenRepr[S, ?], Unit] {

  override def double(d: Unit): GenRepr[S, Double] =
    _ => Generator.Unknown(arbitrary[Double].map(alg.double))

  override def point(genX: GenRepr[S, Double], genY: GenRepr[S, Double]): GenRepr[S, Point] =
    n =>
      for {
        x <- genX(n)
        y <- genY(n)
      } yield alg.point(x, y)

  override def add[T: Semigroup](genA: GenRepr[S, T], genB: GenRepr[S, T]): GenRepr[S, T] =
    n =>
      for {
        a <- genA(n)
        b <- genB(n)
      } yield alg.add(a, b)
}
