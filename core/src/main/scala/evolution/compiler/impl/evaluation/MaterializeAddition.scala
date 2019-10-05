package evolution.compiler.impl.evaluation
import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.expression.typeclass.Additive._
import evolution.materialization.Evolution

object MaterializeAddition {
  def apply[A, B, C](additive: Additive[A, B, C])(a: A, b: B): C =
    additive match {
      case DoubleIntDouble    => a + b
      case DoubleDoubleDouble => a + b
      case IntDoubleDouble    => a + b
      case IntIntInt          => a + b
      case PointPointPoint    => a.plus(b)
      case LiftBoth(add)      => Evolution.zipWithUncurried(MaterializeAddition(add))(a, b)
    }
}
