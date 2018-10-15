package evolution.primitive.algebra.constants
import cats.kernel.Semigroup
import cats.~>
import evolution.geometry.Point

trait Constants[S[_]] {
  def double(d: Double): S[Double]
  def point(x: S[Double], y: S[Double]): S[Point]
  def add[T: Semigroup](a: S[T], b: S[T]): S[T]
}

class MappedConstants[S1[_], S2[_]](alg: Constants[S1], to: S1 ~> S2, from: S2 ~> S1) extends Constants[S2] {
  def double(d: Double): S2[Double] =
    to(alg.double(d))
  def point(x: S2[Double], y: S2[Double]): S2[Point] =
    to(alg.point(from(x), from(y)))
  def add[T: Semigroup](a: S2[T], b: S2[T]): S2[T] =
    to(alg.add(from(a), from(b)))
}

class ContextualConstants[S[_], Ctx](alg: Constants[S]) extends Constants[λ[α => Ctx => S[α]]] {
  override def double(d: Double): Ctx => S[Double] = _ => alg.double(d)
  override def point(x: Ctx => S[Double], y: Ctx => S[Double]): Ctx => S[Point] = ctx => alg.point(x(ctx), y(ctx))
  override def add[T: Semigroup](a: Ctx => S[T], b: Ctx => S[T]): Ctx => S[T] =
    ctx => alg.add(a(ctx), b(ctx))
}