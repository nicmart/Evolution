package evolution.compiler.impl.evaluation

import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass.Multiplicative
import evolution.compiler.expression.typeclass.Multiplicative._

private[evaluation] object MaterializeMultiplication {
  def apply[A, B, C](multiplicative: Multiplicative[A, B, C])(a: A, b: B): C =
    multiplicative match {
      case IntIntInt          => a * b
      case DoubleDoubleDouble => a * b
      case DoublePointPoint   => b.mult(a)
      case IntDoubleDouble    => a * b
      case DoubleIntDouble    => a * b
      case IntPointPoint      => b.mult(a)
      case PointIntPoint      => a.mult(b)
      case PointDoublePoint   => a.mult(b)
      case m: LiftLeft[a, b, c] =>
        val f = MaterializeMultiplication(m.m) _
        Evolution.map[a, c](a, aa => f(aa, b))
      case m: LiftRight[a, b, c] =>
        val f = MaterializeMultiplication(m.m) _
        Evolution.map[b, c](b, bb => f(a, bb))
      case m: LiftBoth[a, b, c] =>
        val f = MaterializeMultiplication(m.m) _
        Evolution.zipWithUncurried[a, b, c]((aa, bb) => f(aa, bb))(a, b)
    }
}
