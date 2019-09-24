package evolution.compiler.impl.evaluation

import evolution.compiler.expression.typeclass.Equable
import cats.kernel.Eq
import cats.implicits._
import evolution.geometry.Point

object MaterializeEquality {
  def apply[T](eq: Equable[T]): Eq[T] = eq match {
    case Equable.Double  => Eq[Double]
    case Equable.Int     => Eq[Int]
    case Equable.Point   => Eq[Point]
    case Equable.Boolean => Eq[Boolean]
  }
}
