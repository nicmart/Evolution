package evolution.primitive.algebra.constants
import cats.kernel.Semigroup
import cats.~>
import evolution.geometry.Point

trait ConstantsAlgebra[S[_]] {
  def double(d: Double): S[Double]
  def point(x: Double, y: Double): S[Point]
  def add[T: Semigroup](a: S[T], b: S[T]): S[T]
}

class MappedConstantsAlgebra[S1[_], S2[_]](alg: ConstantsAlgebra[S1], to: S1 ~> S2, from: S2 ~> S1)
    extends ConstantsAlgebra[S2] {
  def double(d: Double): S2[Double] =
    to(alg.double(d))
  def point(x: Double, y: Double): S2[Point] =
    to(alg.point(x, y))
  def add[T: Semigroup](a: S2[T], b: S2[T]): S2[T] =
    to(alg.add(from(a), from(b)))
}

class ContextualConstantsAlgebra[S[_], Ctx](alg: ConstantsAlgebra[S]) extends ConstantsAlgebra[λ[α => Ctx => S[α]]] {
  override def double(d: Double): Ctx => S[Double] = _ => alg.double(d)
  override def point(x: Double, y: Double): Ctx => S[Point] = _ => alg.point(x, y)
  override def add[T: Semigroup](a: Ctx => S[T], b: Ctx => S[T]): Ctx => S[T] =
    ctx => alg.add(a(ctx), b(ctx))
}
