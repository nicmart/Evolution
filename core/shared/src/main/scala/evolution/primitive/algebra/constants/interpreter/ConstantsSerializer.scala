package evolution.primitive.algebra.constants.interpreter
import cats.Group
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.primitive.algebra.CtxString
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

object ConstantsSerializer extends Constants[CtxString] {
  override def int(n: Int): CtxString[Int] =
    _ => n.toString
  override def double(d: Double): CtxString[Double] =
    _ => d.toString
  override def point(x: CtxString[Double], y: CtxString[Double]): CtxString[Point] =
    ctx => s"point(${x(ctx)}, ${y(ctx)})"
  override def add[T: Semigroup](a: CtxString[T], b: CtxString[T]): CtxString[T] =
    ctx => s"${a(ctx)} + ${b(ctx)}"
  override def inverse[T: Group](a: CtxString[T]): CtxString[T] =
    ctx => s"-${a(ctx)}"
  override def sin(d: CtxString[Double]): CtxString[Double] =
    ctx => s"sin(${d(ctx)})"
  override def cos(d: CtxString[Double]): CtxString[Double] =
    ctx => s"cos(${d(ctx)})"
  override def multiply[T: VectorSpace](k: CtxString[Double], t: CtxString[T]): CtxString[T] =
    ctx => s"multiply(${k(ctx)}, ${t(ctx)})"
  override def eq[T: Eq](a: CtxString[T], b: CtxString[T]): CtxString[Boolean] =
    ctx => s"eq(${a(ctx)}, ${b(ctx)})"
  override def ifThen[T](condition: CtxString[Boolean], a: CtxString[T], b: CtxString[T]): CtxString[T] =
    ctx => s"if(${condition(ctx)}, ${a(ctx)}, ${b(ctx)})"
}
