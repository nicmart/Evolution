package evolution.compiler.expression.typeclass
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Invertible[A]

object Invertible {
  case object Int extends Invertible[Int]
  case object Double extends Invertible[Double]
  case object Point extends Invertible[Point]
  case object DoubleEvo extends Invertible[Evolution[Double]]
  case object PointEvo extends Invertible[Evolution[Point]]
}
