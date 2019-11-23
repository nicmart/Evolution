package evolution.compiler.impl.evaluation
import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.expression.typeclass.Additive._
import evolution.materialization.Evolution
import evolution.geometry.Point

object MaterializeAddition {
  def apply[A, B, C](additive: Additive[A, B, C]): (A, B) => C =
    additive match {
      case DoubleIntDouble    => (a: Double, b: Int) => a + b
      case DoubleDoubleDouble => (a: Double, b: Double) => a + b
      case IntDoubleDouble    => (a: Int, b: Double) => a + b
      case IntIntInt          => (a: Int, b: Int) => a + b
      case PointPointPoint    => (a: Point, b: Point) => a.plus(b)
      case LiftLeft(add) =>
        val f = MaterializeAddition(add)
        (a, b) => Evolution.map(a, aa => f(aa, b))
      case LiftRight(add) =>
        val f = MaterializeAddition(add)
        (a, b) => Evolution.map(b, bb => f(a, bb))
      case LiftBoth(add) =>
        val f = MaterializeAddition(add)
        (a, b) => Evolution.zipWithUncurried(f)(a, b)
    }
}
