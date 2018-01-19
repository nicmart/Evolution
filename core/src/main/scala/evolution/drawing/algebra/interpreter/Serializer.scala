package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.DrawingAlgebra
import evolution.drawing.algebra.DrawingAlgebra.{DoubleType, PointType}
import evolution.geometry.Point

object Serializer extends DrawingAlgebra[CtxString] {
  override def rnd[E](from: Double, to: Double): List[String] => String =
    _ => s"rnd($from,$to)"
  override def point[E](x: List[String] => String, y: List[String] => String): List[String] => String =
    e => s"point(${x(e)},${y(e)})"
  override def polar[E](r: List[String] => String, w: List[String] => String): List[String] => String =
    e => s"polar(${r(e)},${w(e)})"
  override def const[E, T: DrawingAlgebra.Type](x: T): List[String] => String =
    e => DrawingAlgebra.typeInstance[T].fold(x)(
      _.toString,
      point => s"point(${point.x},${point.y})"
    )
  override def integrate[E, T: DrawingAlgebra.Type](start: T, f: List[String] => String): List[String] => String =
    e => s"integrate(${const(start).apply(e)},${f(e)})"
  override def derive[E, T: DrawingAlgebra.Type](f: List[String] => String): List[String] => String =
    e => s"derive(${f(e)})"
  override def var0[E, A]: CtxString[(CtxString[E, A], E), A] =
    ctx => "$" + ctx.headOption.getOrElse("x")
  override def shift[E, A, B](expr: CtxString[E, A]): CtxString[(CtxString[E, B], E), A] =
    ctx => expr(ctx.tail)
  override def let[E, A, B](name: String, value: CtxString[E, A])(expr: CtxString[(CtxString[E, A], E), B]): CtxString[E, B] =
    ctx => s"let($name, ${value(ctx)}, ${expr(name :: ctx)})"
}
