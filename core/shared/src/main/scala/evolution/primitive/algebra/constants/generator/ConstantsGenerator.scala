package evolution.primitive.algebra.constants.generator
import cats.kernel.Semigroup
import evolution.generator.Generator
import evolution.geometry.Point
import evolution.primitive.algebra.{ Composed, GenRepr }
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace
import org.scalacheck.Arbitrary.arbitrary

// TODO this can be a an applicative lifted algebra, with an overridden double method
class ConstantsGenerator[S[_]](alg: Constants[S, Double]) extends Constants[GenRepr[S, ?], Unit] {

  override def double(d: Unit): GenRepr[S, Double] =
    _ => Generator.Unknown(arbitrary[Double].map(alg.double))

  override def point(genX: GenRepr[S, Double], genY: GenRepr[S, Double]): GenRepr[S, Point] =
    n =>
      for {
        x <- genX(n)
        y <- genY(n)
      } yield alg.point(x, y)

  override def add[T: VectorSpace](genA: GenRepr[S, T], genB: GenRepr[S, T]): GenRepr[S, T] =
    n =>
      for {
        a <- genA(n)
        b <- genB(n)
      } yield alg.add(a, b)

  override def sin(gen: GenRepr[S, Double]): GenRepr[S, Double] =
    n =>
      for {
        d <- gen(n)
      } yield alg.sin(d)

  override def cos(gen: GenRepr[S, Double]): GenRepr[S, Double] =
    n =>
      for {
        d <- gen(n)
      } yield alg.cos(d)

  override def multiply[T: VectorSpace](genK: GenRepr[S, Double], genT: GenRepr[S, T]): GenRepr[S, T] =
    n =>
      for {
        k <- genK(n)
        t <- genT(n)
      } yield alg.multiply(k, t)
}
