package evolution.compiler.expression.typeclass

sealed trait Numeric[T]

object Numeric {
  case object Int extends Numeric[Int]
  case object Double extends Numeric[Double]
}
