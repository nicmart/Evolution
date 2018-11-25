package evolution.primitive.algebra.derived.interpreter
import evolution.geometry.Point
import evolution.primitive.algebra.CtxString
import evolution.primitive.algebra.derived.Derived

class DerivedSerializer[F[_]] extends Derived[F, CtxString] {
  override def cartesian(x: CtxString[F[Double]], y: CtxString[F[Double]]): CtxString[F[Point]] =
    ctx => s"cartesian(${x(ctx)}, ${y(ctx)})"

  override def constant[A](a: CtxString[A]): CtxString[F[A]] =
    ctx => s"constant(${a(ctx)})"
}
