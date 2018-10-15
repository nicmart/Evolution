package evolution.primitive.algebra.constants.generator
import cats.kernel.Semigroup
import evolution.generator.Generator
import evolution.geometry.Point
import evolution.primitive.algebra.{Composed, GenRepr}
import evolution.primitive.algebra.constants.Constants
import org.scalacheck.Gen

// TODO this can be a an applicative lifted algebra, because
// there are no ad-hoc operations using the input of the function (unlike binding algebra,
// for which the representation GenRepr was choosen
class ConstantsGenerator[S[_]](alg: Constants[S]) extends Constants[GenRepr[S, ?]] {

  override def double(d: Double): GenRepr[S, Double] =
    _ => Generator.pure(alg.double(d))

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
