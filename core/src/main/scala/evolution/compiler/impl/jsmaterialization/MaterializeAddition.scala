package evolution.compiler.impl.jsmaterialization
import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode.JsExpr
import evolution.compiler.expression.typeclass.Additive._

object MaterializeAddition {
  def apply[A, B, C](add: Additive[A, B, C])(a: JsExpr, b: JsExpr): JsExpr = add match {
    case IntDoubleDouble    => JsExpr.BinaryOp(a, "+", b)
    case DoubleDoubleDouble => JsExpr.BinaryOp(a, "+", b)
    case DoubleIntDouble    => JsExpr.BinaryOp(a, "+", b)
    case PointPointPoint    => JsExpr.App(JsExpr.Select(a, "plus"), List(b))
    case IntIntInt          => JsExpr.BinaryOp(a, "+", b)
    case LiftLeft(add)      => MaterializeJsCode.mapIterable(a, aa => MaterializeAddition(add)(aa, b))
    case LiftRight(add)     => MaterializeJsCode.mapIterable(b, bb => MaterializeAddition(add)(a, bb))
    case LiftBoth(add)      => MaterializeJsCode.zipIterable(a, b, MaterializeAddition(add))
  }
}
