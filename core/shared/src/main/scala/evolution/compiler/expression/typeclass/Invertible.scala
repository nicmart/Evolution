package evolution.compiler.expression.typeclass
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Invertible[A]

object Invertible {
  case object IntInvertible extends Invertible[Int]
  case object DblInvertible extends Invertible[Double]
  case object PointInvertible extends Invertible[Point]
  case object DblEvoInvertible extends Invertible[Evolution[Double]]
  case object PointEvoInvertible extends Invertible[Evolution[Point]]
}
