package evolution.compiler.impl.evaluation
import evolution.materialization.Evolution
import evolution.geometry.Point
import evolution.compiler.expression.typeclass.Invertible
import evolution.compiler.expression.typeclass.Invertible.DblInvertible
import evolution.compiler.expression.typeclass.Invertible.IntInvertible
import evolution.compiler.expression.typeclass.Invertible.DblEvoInvertible
import evolution.compiler.expression.typeclass.Invertible.PointInvertible
import evolution.compiler.expression.typeclass.Invertible.PointEvoInvertible

object MaterializeInverse {
  def apply[T](inv: Invertible[T])(t: T): T = inv match {
    case DblInvertible      => -t
    case IntInvertible      => -t
    case DblEvoInvertible   => Evolution.map(t, (a: Double) => -a)
    case PointInvertible    => t.opposite
    case PointEvoInvertible => Evolution.map(t, (a: Point) => a.opposite)
  }
}
