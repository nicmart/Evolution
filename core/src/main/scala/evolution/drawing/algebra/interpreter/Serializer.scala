package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.DrawingAlgebra
import evolution.drawing.algebra.DrawingAlgebra.{DoubleType, PointType}
import evolution.geometry.Point

object Serializer extends DrawingAlgebra[CtxString] {
  override def rnd[E](from: Double, to: Double): E => String =
    _ => s"rnd($from,$to)"
  override def point[E](x: E => String, y: E => String): E => String =
    e => s"point(${x(e)},${y(e)})"
  override def polar[E](r: E => String, w: E => String): E => String =
    e => s"polar(${r(e)},${w(e)})"
  override def const[E, T: DrawingAlgebra.Type](x: T): E => String =
    e => DrawingAlgebra.typeInstance[T].fold(x)(
      _.toString,
      point => s"point(${point.x},${point.y})"
    )
  override def integrate[E, T: DrawingAlgebra.Type](start: T, f: E => String): E => String =
    e => s"integrate(${const(start).apply(e)},${f(e)})"
  override def derive[E, T: DrawingAlgebra.Type](f: E => String): E => String =
    e => s"derive(${f(e)})"
}
