package evolution.compiler.impl.jsmaterialization
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode.JsExpr
import evolution.compiler.expression.typeclass.Invertible

object MaterializeInverse {
  def apply[T](inv: Invertible[T])(t: JsExpr): JsExpr = inv match {
    case Invertible.Double    => JsExpr.PrefixOp("-", t)
    case Invertible.Int       => JsExpr.PrefixOp("-", t)
    case Invertible.Point     => JsExpr.Select(t, "opposite")
    case Invertible.Lift(inv) => MaterializeJsCode.mapIterable(t, MaterializeInverse(inv))
  }
}
