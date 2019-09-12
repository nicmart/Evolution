package evolution.compiler.impl.jsmaterialization
import evolution.compiler.expression.typeclass.Multiplicative
import evolution.compiler.expression.typeclass.Multiplicative.DoubleIntDouble
import evolution.compiler.expression.typeclass.Multiplicative.LiftBoth
import evolution.compiler.expression.typeclass.Multiplicative.IntDoubleDouble
import evolution.compiler.expression.typeclass.Multiplicative.PointDoublePoint
import evolution.compiler.expression.typeclass.Multiplicative.PointIntPoint
import evolution.compiler.expression.typeclass.Multiplicative.IntPointPoint
import evolution.compiler.expression.typeclass.Multiplicative.IntIntInt
import evolution.compiler.expression.typeclass.Multiplicative.DoubleDoubleDouble
import evolution.compiler.expression.typeclass.Multiplicative.LiftLeft
import evolution.compiler.expression.typeclass.Multiplicative.LiftRight
import evolution.compiler.expression.typeclass.Multiplicative.DoublePointPoint
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode.JsExpr

object MaterializeMultiplication {
  def apply[A, B, C](mult: Multiplicative[A, B, C])(a: JsExpr, b: JsExpr): JsExpr = mult match {
    case DoubleIntDouble    => ???
    case LiftBoth(m)        => ???
    case IntDoubleDouble    => ???
    case PointDoublePoint   => ???
    case PointIntPoint      => ???
    case IntPointPoint      => ???
    case IntIntInt          => ???
    case DoubleDoubleDouble => ???
    case LiftLeft(m)        => ???
    case LiftRight(m)       => ???
    case DoublePointPoint   => ???
  }
}
