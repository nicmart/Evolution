package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.DrawingAlgebra
import evolution.geometry.Point

object Serializer extends DrawingAlgebra[ConstString] {
  override def rnd(from: Double, to: Double) =
    s"rnd($from,$to)"
  override def const(x: Double) =
    s"const($x)"
  override def cartesian(x: String, y: String) =
    s"cartesian($x,$y)"
  override def polar(r: String, w: String) =
    s"polar($r,$w)"
  override def integrateDouble(start: Double, f: String) =
    s"integrateDouble($start,$f)"
  override def integratePoint(start: Point, f: String) =
    s"integratePoint($start,$f)"
  override def deriveDouble(f: String) =
    s"deriveDouble($f)"
  override def derivePoint(f: String) =
    s"derivePoint($f)"
}
