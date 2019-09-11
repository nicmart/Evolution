package evolution.compiler.expression.typeclass
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Multiplicative[A, B, C]

object Multiplicative {
  case object DblDblDbl extends Multiplicative[Double, Double, Double]
  case object DblPointPoint extends Multiplicative[Double, Point, Point]
  case object PointDblPoint extends Multiplicative[Point, Double, Point]
  case object IntIntInt extends Multiplicative[Int, Int, Int]
  case object IntDblDbl extends Multiplicative[Int, Double, Double]
  case object DblIntDbl extends Multiplicative[Double, Int, Double]
  case object IntPointPoint extends Multiplicative[Int, Point, Point]
  case object PointIntPoint extends Multiplicative[Point, Int, Point]
  case object DblEvoDblEvoDbl extends Multiplicative[Double, Evolution[Double], Evolution[Double]]
  case object EvoDblDblEvoDbl extends Multiplicative[Evolution[Double], Double, Evolution[Double]]
  case object DblEvoPointEvoPoint extends Multiplicative[Double, Evolution[Point], Evolution[Point]]
  case object EvoPointDblEvoPoint extends Multiplicative[Evolution[Point], Double, Evolution[Point]]
  case object EvoDblEvoDblEvoDbl extends Multiplicative[Evolution[Double], Evolution[Double], Evolution[Double]]
  case object EvoPointEvoDblEvoPoint extends Multiplicative[Evolution[Point], Evolution[Double], Evolution[Point]]
  case object EvoDblEvoPointEvoPoint extends Multiplicative[Evolution[Double], Evolution[Point], Evolution[Point]]
}
