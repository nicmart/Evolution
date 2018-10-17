package evolution.primitive.algebra.constants
import cats.kernel.Semigroup
import cats.~>
import evolution.geometry.Point

trait Constants[S[_], D] {
  def double(d: D): S[Double]
  def point(x: S[Double], y: S[Double]): S[Point]
  def add[T: Semigroup](a: S[T], b: S[T]): S[T]
}

class MappedConstants[S1[_], S2[_], D](alg: Constants[S1, D], to: S1 ~> S2, from: S2 ~> S1) extends Constants[S2, D] {
  def double(d: D): S2[Double] =
    to(alg.double(d))
  def point(x: S2[Double], y: S2[Double]): S2[Point] =
    to(alg.point(from(x), from(y)))
  def add[T: Semigroup](a: S2[T], b: S2[T]): S2[T] =
    to(alg.add(from(a), from(b)))
}

// TODO Constants Applicative?
class ContextualConstants[S[_], D, Ctx](alg: Constants[S, D]) extends Constants[λ[α => Ctx => S[α]], D] {
  override def double(d: D): Ctx => S[Double] = _ => alg.double(d)
  override def point(x: Ctx => S[Double], y: Ctx => S[Double]): Ctx => S[Point] = ctx => alg.point(x(ctx), y(ctx))
  override def add[T: Semigroup](a: Ctx => S[T], b: Ctx => S[T]): Ctx => S[T] =
    ctx => alg.add(a(ctx), b(ctx))
}
