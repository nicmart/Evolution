package evolution.compiler.impl.evaluation
import evolution.materialization.Evolution
import evolution.geometry.Point
import evolution.compiler.expression.typeclass.Invertible

object MaterializeInverse {
  def apply[T](inv: Invertible[T])(t: T): T = inv match {
    case Invertible.Double    => -t
    case Invertible.Int       => -t
    case Invertible.DoubleEvo => Evolution.map(t, (a: Double) => -a)
    case Invertible.Point     => t.opposite
    case Invertible.PointEvo  => Evolution.map(t, (a: Point) => a.opposite)
  }
}
