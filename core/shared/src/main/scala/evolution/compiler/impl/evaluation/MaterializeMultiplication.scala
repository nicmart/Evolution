package evolution.compiler.impl.evaluation

import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass.Multiplicative
import evolution.compiler.expression.typeclass.Multiplicative._
import evolution.geometry.Point

object MaterializeMultiplication {
  def apply[A, B, C](multiplicative: Multiplicative[A, B, C])(a: A, b: B): C =
    multiplicative match {
      case IntIntInt                   => a * b
      case DoubleDoubleDouble          => a * b
      case DoublePointPoint            => b.mult(a)
      case IntDoubleDouble             => a * b
      case DoubleIntDouble             => a * b
      case IntPointPoint               => b.mult(a)
      case PointIntPoint               => a.mult(b)
      case PointDoublePoint            => a.mult(b)
      case EvoDoubleDoubleEvoDouble    => Evolution.map[Double, Double](a, _ * b)
      case DoubleEvoPointEvoPoint      => Evolution.map[Point, Point](b, _.mult(a))
      case EvoPointEvoDoubleEvoPoint   => Evolution.zipWithUncurried[Point, Double, Point]((a, b) => a * b)(a, b)
      case EvoPointDoubleEvoPoint      => Evolution.map[Point, Point](a, _.mult(b))
      case DoubleEvoDoubleEvoDouble    => Evolution.map[Double, Double](b, _ * a)
      case EvoDoubleEvoPointEvoPoint   => Evolution.zipWithUncurried[Double, Point, Point]((a, b) => b * a)(a, b)
      case EvoDoubleEvoDoubleEvoDouble => Evolution.zipWithUncurried[Double, Double, Double]((a, b) => a * b)(a, b)
    }
}
