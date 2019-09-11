package evolution.compiler.expression.typeclass
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Additive[A, B, C]

object Additive {
  case object DblDblDbl extends Additive[Double, Double, Double]
  case object IntIntInt extends Additive[Int, Int, Int]
  case object IntDblDbl extends Additive[Int, Double, Double]
  case object DblIntDbl extends Additive[Double, Int, Double]
  case object PointPointPoint extends Additive[Point, Point, Point]
  case object EvoDblEvoDblEvoDbl extends Additive[Evolution[Double], Evolution[Double], Evolution[Double]]
  case object EvoPointEvoPointEvoPoint extends Additive[Evolution[Point], Evolution[Point], Evolution[Point]]
}
