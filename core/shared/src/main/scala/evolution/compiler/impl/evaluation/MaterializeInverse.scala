package evolution.compiler.impl.evaluation
import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass.Invertible

object MaterializeInverse {
  def apply[T](inv: Invertible[T])(t: T): T = inv match {
    case Invertible.Double    => -t
    case Invertible.Int       => -t
    case Invertible.Point     => t.opposite
    case Invertible.Lift(inv) => Evolution.map(t, MaterializeInverse(inv))
  }
}
