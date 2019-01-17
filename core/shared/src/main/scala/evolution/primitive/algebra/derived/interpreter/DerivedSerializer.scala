package evolution.primitive.algebra.derived.interpreter
import evolution.geometry.Point
import evolution.primitive.algebra.CtxString
import evolution.primitive.algebra.derived.Derived
import evolution.typeclass.VectorSpace

class DerivedSerializer[F[_]] extends Derived[F, CtxString] {
  override def cartesian(x: CtxString[F[Double]], y: CtxString[F[Double]]): CtxString[F[Point]] =
    ctx => s"cartesian(${x(ctx)}, ${y(ctx)})"

  override def constant[A](a: CtxString[A]): CtxString[F[A]] =
    ctx => s"${a(ctx)}"

  override def polar(radius: CtxString[F[Double]], angle: CtxString[F[Double]]): CtxString[F[Point]] =
    ctx => s"polar(${radius(ctx)}, ${angle(ctx)})"

  override def integrate[A: VectorSpace](start: CtxString[A], speed: CtxString[F[A]]): CtxString[F[A]] =
    ctx => s"integrate(${start(ctx)}, ${speed(ctx)})"

  override def solve1[X: VectorSpace](eq: CtxString[F[X => X]], x0: CtxString[X]): CtxString[F[X]] =
    ctx => s"solve1(${eq(ctx)}, ${x0(ctx)})"

  override def map[A, B](fa: CtxString[F[A]], f: CtxString[A => B]): CtxString[F[B]] =
    ctx => s"map(${fa(ctx)}, ${f(ctx)})"

  override def concat[A](fa1: CtxString[F[A]], fa2: CtxString[F[A]]): CtxString[F[A]] =
    ctx => s"concat(${fa1(ctx)}, ${fa2(ctx)})"

  override def flatMap[A, B](fa: CtxString[F[A]], f: CtxString[A => F[B]]): CtxString[F[B]] =
    ctx => s"flatMap(${fa(ctx)}, ${f(ctx)})"

  override def take[T](n: CtxString[Int], ft: CtxString[F[T]]): CtxString[F[T]] =
    ctx => s"take(${n(ctx)}, ${ft(ctx)})"
}
