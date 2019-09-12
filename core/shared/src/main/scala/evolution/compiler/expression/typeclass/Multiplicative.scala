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

  case class LiftLeft[A, B, C](m: Multiplicative[A, B, C]) extends Multiplicative[Evolution[A], B, Evolution[C]]
  case class LiftRight[A, B, C](m: Multiplicative[A, B, C]) extends Multiplicative[A, Evolution[B], Evolution[C]]
  case class LiftBoth[A, B, C](m: Multiplicative[A, B, C])
      extends Multiplicative[Evolution[A], Evolution[B], Evolution[C]]
}
