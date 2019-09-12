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
    case IntIntInt          => JsExpr.BinaryOp(a, "*", b)
    case DoubleDoubleDouble => JsExpr.BinaryOp(a, "*", b)
    case DoubleIntDouble    => JsExpr.BinaryOp(a, "*", b)
    case DoublePointPoint   => JsExpr.App(JsExpr.Select(b, "mult"), List(a))
    case IntPointPoint      => JsExpr.App(JsExpr.Select(b, "mult"), List(a))
    case IntDoubleDouble    => JsExpr.BinaryOp(a, "*", b)
    case PointDoublePoint   => JsExpr.App(JsExpr.Select(a, "mult"), List(b))
    case PointIntPoint      => JsExpr.App(JsExpr.Select(a, "mult"), List(b))
    case LiftLeft(m)        => MaterializeJsCode.mapIterable(a, aa => MaterializeMultiplication(m)(aa, b))
    case LiftRight(m)       => MaterializeJsCode.mapIterable(b, bb => MaterializeMultiplication(m)(a, bb))
    case LiftBoth(m)        => MaterializeJsCode.zipIterable(a, b, MaterializeMultiplication(m))
  }
}
