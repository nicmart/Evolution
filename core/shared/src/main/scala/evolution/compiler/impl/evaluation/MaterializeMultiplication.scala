package evolution.compiler.impl.evaluation

import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass.Multiplicative
import evolution.compiler.expression.typeclass.Multiplicative._
import evolution.geometry.Point

object MaterializeMultiplication {
  def apply[A, B, C](multiplicative: Multiplicative[A, B, C])(a: A, b: B): C =
    multiplicative match {
      case IntIntInt              => a * b
      case DblDblDbl              => a * b
      case DblPointPoint          => b.mult(a)
      case IntDblDbl              => a * b
      case DblIntDbl              => a * b
      case IntPointPoint          => b.mult(a)
      case PointIntPoint          => a.mult(b)
      case PointDblPoint          => a.mult(b)
      case EvoDblDblEvoDbl        => Evolution.map[Double, Double](a, _ * b)
      case DblEvoPointEvoPoint    => Evolution.map[Point, Point](b, _.mult(a))
      case EvoPointEvoDblEvoPoint => Evolution.zipWithUncurried[Point, Double, Point]((a, b) => a * b)(a, b)
      case EvoPointDblEvoPoint    => Evolution.map[Point, Point](a, _.mult(b))
      case DblEvoDblEvoDbl        => Evolution.map[Double, Double](b, _ * a)
      case EvoDblEvoPointEvoPoint => Evolution.zipWithUncurried[Double, Point, Point]((a, b) => b * a)(a, b)
      case EvoDblEvoDblEvoDbl     => Evolution.zipWithUncurried[Double, Double, Double]((a, b) => a * b)(a, b)
    }
}
