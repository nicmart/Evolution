package evolution.compiler.impl.evaluation
import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.expression.typeclass.Additive.PointPointPoint
import evolution.compiler.expression.typeclass.Additive.EvoPointEvoPointEvoPoint
import evolution.compiler.expression.typeclass.Additive.DblIntDbl
import evolution.compiler.expression.typeclass.Additive.DblDblDbl
import evolution.compiler.expression.typeclass.Additive.EvoDblEvoDblEvoDbl
import evolution.compiler.expression.typeclass.Additive.IntDblDbl
import evolution.compiler.expression.typeclass.Additive.IntIntInt
import evolution.materialization.Evolution
import evolution.geometry.Point

object MaterializeAddition {
  def apply[A, B, C](additive: Additive[A, B, C])(a: A, b: B): C =
    additive match {
      case DblIntDbl       => a + b
      case DblDblDbl       => a + b
      case IntDblDbl       => a + b
      case IntIntInt       => a + b
      case PointPointPoint => a.plus(b)
      case EvoPointEvoPointEvoPoint =>
        Evolution.zipWithUncurried[Point, Point, Point]((x, y) => x + y)(a, b)
      case EvoDblEvoDblEvoDbl =>
        Evolution.zipWithUncurried[Double, Double, Double]((x, y) => x + y)(a, b)
    }
}
