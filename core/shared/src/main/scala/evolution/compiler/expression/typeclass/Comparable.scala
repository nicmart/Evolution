package evolution.compiler.expression.typeclass

sealed trait Comparable[T]

object Comparable {
  case object Int extends Comparable[Int]
  case object Double extends Comparable[Double]
}
