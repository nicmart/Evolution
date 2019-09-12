package evolution.compiler.expression.typeclass
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Invertible[A]

object Invertible {
  case object Int extends Invertible[Int]
  case object Double extends Invertible[Double]
  case object Point extends Invertible[Point]
  case class Lift[T](inv: Invertible[T]) extends Invertible[Evolution[T]]
}
