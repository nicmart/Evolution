package evolution.compiler.impl.evaluation
import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.expression.typeclass.Additive._
import evolution.materialization.Evolution
import evolution.geometry.Point
import sourcecode.Text.generate

object MaterializeAddition {
  def apply[A, B, C](additive: Additive[A, B, C]): (A, B) => C =
    additive match {
      case DoubleIntDouble    => (a: Double, b: Int) => a + b
      case DoubleDoubleDouble => (a: Double, b: Double) => a + b
      case IntDoubleDouble    => (a: Int, b: Double) => a + b
      case IntIntInt          => (a: Int, b: Int) => a + b
      case PointPointPoint    => (a: Point, b: Point) => a.plus(b)
      case LiftLeft(add) =>
        val f = MaterializeAddition(add.asInstanceOf[Additive[Any, Any, Any]])
        (a, b) => Evolution.map[Any, Any](a, aa => f(aa, b)).asInstanceOf[C]
      case LiftRight(add) =>
        val f = MaterializeAddition(add.asInstanceOf[Additive[Any, Any, Any]])
        (a, b) => Evolution.map[Any, Any](b, bb => f(a, bb)).asInstanceOf[C]
      case LiftBoth(add) =>
        val f = MaterializeAddition(add.asInstanceOf[Additive[Any, Any, Any]])
        (a, b) => Evolution.zipWithUncurried[Any, Any, Any](f)(a, b).asInstanceOf[C]
    }
}
