package evolution.compiler.impl.evaluation
import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.expression.typeclass.Additive.PointPointPoint
import evolution.compiler.expression.typeclass.Additive.EvoPointEvoPointEvoPoint
import evolution.compiler.expression.typeclass.Additive.DoubleIntDouble
import evolution.compiler.expression.typeclass.Additive.DoubleDoubleDouble
import evolution.compiler.expression.typeclass.Additive.EvoDoubleEvoDoubleEvoDouble
import evolution.compiler.expression.typeclass.Additive.IntDoubleDouble
import evolution.compiler.expression.typeclass.Additive.IntIntInt
import evolution.materialization.Evolution
import evolution.geometry.Point

object MaterializeAddition {
  def apply[A, B, C](additive: Additive[A, B, C])(a: A, b: B): C =
    additive match {
      case DoubleIntDouble    => a + b
      case DoubleDoubleDouble => a + b
      case IntDoubleDouble    => a + b
      case IntIntInt          => a + b
      case PointPointPoint    => a.plus(b)
      case EvoPointEvoPointEvoPoint =>
        Evolution.zipWithUncurried[Point, Point, Point]((x, y) => x + y)(a, b)
      case EvoDoubleEvoDoubleEvoDouble =>
        Evolution.zipWithUncurried[Double, Double, Double]((x, y) => x + y)(a, b)
    }
}
