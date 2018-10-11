package evolution.primitive.algebra.constants.generator
import cats.kernel.Semigroup
import evolution.generator.Generator
import evolution.geometry.Point
import evolution.primitive.algebra.{Composed, GenRepr}
import evolution.primitive.algebra.constants.ConstantsAlgebra
import org.scalacheck.Gen

class ConstantsAlgebraGenerator[S[_]](alg: ConstantsAlgebra[S]) extends ConstantsAlgebra[GenRepr[S, ?]] {

  override def double(d: Double): GenRepr[S, Double] =
    _ => Generator.pure(alg.double(d))

  override def point(genX: GenRepr[S, Double], genY: GenRepr[S, Double]): GenRepr[S, Point] =
    n =>
      for {
        x <- genX(n).resize(_ / 2)
        y <- genY(n).resize(_ / 2)
      } yield alg.point(x, y)

  override def add[T: Semigroup](genA: GenRepr[S, T], genB: GenRepr[S, T]): GenRepr[S, T] =
    n =>
      for {
        a <- genA(n).resize(_ / 2)
        b <- genB(n).resize(_ / 2)
      } yield alg.add(a, b)
}
