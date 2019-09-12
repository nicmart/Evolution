package evolution.compiler.impl.evaluation

import evolution.compiler.expression.typeclass.Equable
import cats.kernel.Eq
import cats.implicits._
import evolution.compiler.expression.typeclass.Equable._
import evolution.geometry.Point

object MaterializeEquality {
  def apply[T](eq: Equable[T]): Eq[T] = eq match {
    case DblEquable   => Eq[Double]
    case IntEquable   => Eq[Int]
    case PointEquable => Eq[Point]
    case BoolEquable  => Eq[Boolean]
  }
}
