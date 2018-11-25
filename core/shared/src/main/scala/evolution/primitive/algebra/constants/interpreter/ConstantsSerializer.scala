package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.CtxString
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

object ConstantsSerializer extends Constants[CtxString, Double] {
  override def double(d: Double): CtxString[Double] =
    _ => d.toString
  override def point(x: CtxString[Double], y: CtxString[Double]): CtxString[Point] =
    ctx => s"point(${x(ctx)}, ${y(ctx)})"
  override def add[T: VectorSpace](a: CtxString[T], b: CtxString[T]): CtxString[T] =
    ctx => s"add(${a(ctx)}, ${b(ctx)})"
  override def sin(d: CtxString[Double]): CtxString[Double] =
    ctx => s"sin(${d(ctx)})"
  override def cos(d: CtxString[Double]): CtxString[Double] =
    ctx => s"cos(${d(ctx)})"
}
