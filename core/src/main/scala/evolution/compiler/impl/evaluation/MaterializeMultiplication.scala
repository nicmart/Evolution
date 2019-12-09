package evolution.compiler.impl.evaluation

import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass.Multiplicative
import evolution.compiler.expression.typeclass.Multiplicative._
import evolution.geometry.Point

object MaterializeMultiplication {
  def apply[A, B, C](multiplicative: Multiplicative[A, B, C]): (A, B) => C =
    multiplicative match {
      case IntIntInt          => (a: Int, b: Int) => a * b
      case DoubleDoubleDouble => (a: Double, b: Double) => a * b
      case DoublePointPoint   => (a: Double, b: Point) => b.mult(a)
      case IntDoubleDouble    => (a: Int, b: Double) => a * b
      case DoubleIntDouble    => (a: Double, b: Int) => a * b
      case IntPointPoint      => (a: Int, b: Point) => b.mult(a)
      case PointIntPoint      => (a: Point, b: Int) => a.mult(b)
      case PointDoublePoint   => (a: Point, b: Double) => a.mult(b)
      case LiftLeft(m) =>
        val f = MaterializeMultiplication(m)
        (a, b) => Evolution.map(a, aa => f(aa, b))
      case LiftRight(m) =>
        val f = MaterializeMultiplication(m)
        (a, b) => Evolution.map(b, bb => f(a, bb))
      case LiftBoth(m) =>
        val f = MaterializeMultiplication(m)
        (a, b) => Evolution.zipWithUncurried(f)(a, b)
    }
}
