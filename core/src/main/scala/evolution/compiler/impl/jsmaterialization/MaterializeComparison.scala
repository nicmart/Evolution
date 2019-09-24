package evolution.compiler.impl.jsmaterialization
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode.JsExpr
import evolution.compiler.expression.typeclass.Comparable

object MaterializeComparison {
  def apply[T](cmp: Comparable[T]): Comparison = cmp match {
    case Comparable.Double => new Comparison
    case Comparable.Int    => new Comparison
  }

  class Comparison {
    def gt(a: JsExpr, b: JsExpr): JsExpr = JsExpr.BinaryOp(a, ">", b)
    def gteqv(a: JsExpr, b: JsExpr): JsExpr = JsExpr.BinaryOp(a, ">=", b)
    def lt(a: JsExpr, b: JsExpr): JsExpr = JsExpr.BinaryOp(a, "<", b)
    def lteqv(a: JsExpr, b: JsExpr): JsExpr = JsExpr.BinaryOp(a, "<=", b)
  }
}
