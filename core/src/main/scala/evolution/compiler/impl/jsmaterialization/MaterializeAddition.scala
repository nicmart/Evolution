package evolution.compiler.impl.jsmaterialization
import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode.JsExpr
import evolution.compiler.expression.typeclass.Additive.IntDoubleDouble
import evolution.compiler.expression.typeclass.Additive.DoubleDoubleDouble
import evolution.compiler.expression.typeclass.Additive.DoubleIntDouble
import evolution.compiler.expression.typeclass.Additive.Pointwise
import evolution.compiler.expression.typeclass.Additive.PointPointPoint
import evolution.compiler.expression.typeclass.Additive.IntIntInt

object MaterializeAddition {
  def apply[A, B, C](add: Additive[A, B, C])(a: JsExpr, b: JsExpr): JsExpr = add match {
    case IntDoubleDouble    => JsExpr.BinaryOp(a, "+", b)
    case DoubleDoubleDouble => JsExpr.BinaryOp(a, "+", b)
    case DoubleIntDouble    => JsExpr.BinaryOp(a, "+", b)
    case PointPointPoint    => JsExpr.App(JsExpr.Select(a, "plus"), List(b))
    case IntIntInt          => JsExpr.BinaryOp(a, "+", b)
    case Pointwise(add)     => MaterializeJsCode.zipIterable(a, b, MaterializeAddition(add))
  }
}
