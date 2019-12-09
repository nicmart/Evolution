package evolution.compiler.impl.evaluation

import evolution.compiler.expression.typeclass.Comparable
import cats.kernel.Order
import cats.implicits._

object MaterializeComparison {
  def apply[T](comparable: Comparable[T]): Order[T] = comparable match {
    case Comparable.Double => Order[Double]
    case Comparable.Int    => Order[Int]
  }
}
