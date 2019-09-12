package evolution.compiler.expression.typeclass
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Multiplicative[A, B, C]

object Multiplicative {
  case object DoubleDoubleDouble extends Multiplicative[Double, Double, Double]
  case object DoublePointPoint extends Multiplicative[Double, Point, Point]
  case object PointDoublePoint extends Multiplicative[Point, Double, Point]
  case object IntIntInt extends Multiplicative[Int, Int, Int]
  case object IntDoubleDouble extends Multiplicative[Int, Double, Double]
  case object DoubleIntDouble extends Multiplicative[Double, Int, Double]
  case object IntPointPoint extends Multiplicative[Int, Point, Point]
  case object PointIntPoint extends Multiplicative[Point, Int, Point]
  case object DoubleEvoDoubleEvoDouble extends Multiplicative[Double, Evolution[Double], Evolution[Double]]
  case object EvoDoubleDoubleEvoDouble extends Multiplicative[Evolution[Double], Double, Evolution[Double]]
  case object DoubleEvoPointEvoPoint extends Multiplicative[Double, Evolution[Point], Evolution[Point]]
  case object EvoPointDoubleEvoPoint extends Multiplicative[Evolution[Point], Double, Evolution[Point]]
  case object EvoDoubleEvoDoubleEvoDouble
      extends Multiplicative[Evolution[Double], Evolution[Double], Evolution[Double]]
  case object EvoPointEvoDoubleEvoPoint extends Multiplicative[Evolution[Point], Evolution[Double], Evolution[Point]]
  case object EvoDoubleEvoPointEvoPoint extends Multiplicative[Evolution[Double], Evolution[Point], Evolution[Point]]
}
