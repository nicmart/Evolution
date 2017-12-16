package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.DrawingAlgebra
import evolution.drawing.algebra.DrawingAlgebra.{DoubleType, PointType}
import evolution.geometry.Point

object Serializer extends DrawingAlgebra[ConstString] {
  override def rnd(from: Double, to: Double) =
    s"rnd($from,$to)"
  override def cartesian(x: String, y: String) =
    s"cartesian($x,$y)"
  override def polar(r: String, w: String) =
    s"polar($r,$w)"
  override def const[T: DrawingAlgebra.Type](x: T): String =
    DrawingAlgebra.typeInstance[T].fold(x)(
      _.toString,
      point => s"point(${point.x},${point.y})"
    )
  override def integrate[T: DrawingAlgebra.Type](start: T, f: ConstString[T]): String =
    s"integrate(${const(start)},$f)"
  override def derive[T: DrawingAlgebra.Type](f: ConstString[T]) =
    s"derive($f)"
}
