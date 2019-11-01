package evolution.compiler.impl.evaluation
import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass.Invertible
import evolution.geometry.Point

private[evaluation] object MaterializeInverse {
  def apply[T](inv: Invertible[T]): T => T = inv match {
    case Invertible.Double => (t: Double) => -t
    case Invertible.Int    => (t: Int) => -t
    case Invertible.Point  => (t: Point) => t.opposite
    case Invertible.Lift(inv) =>
      val f = MaterializeInverse(inv)
      t => Evolution.map(t, f)
  }
}
