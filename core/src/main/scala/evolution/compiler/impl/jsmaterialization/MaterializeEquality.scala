package evolution.compiler.impl.jsmaterialization
import evolution.compiler.expression.typeclass.Equable
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode.JsExpr

object MaterializeEquality {
  def apply[T](eq: Equable[T]): Equality = eq match {
    case Equable.Boolean => new DefaultEquality
    case Equable.Double  => new DefaultEquality
    case Equable.Int     => new DefaultEquality
    case Equable.Point   => new PointEquality
  }

  trait Equality {
    def eqv(a: JsExpr, b: JsExpr): JsExpr = ???
    def neqv(a: JsExpr, b: JsExpr): JsExpr = ???
  }

  class DefaultEquality extends Equality {
    override def eqv(a: JsExpr, b: JsExpr): JsExpr = JsExpr.BinaryOp(a, "==", b)
    override def neqv(a: JsExpr, b: JsExpr): JsExpr = JsExpr.BinaryOp(a, "!=", b)
  }

  class PointEquality extends Equality {
    override def eqv(a: JsExpr, b: JsExpr): JsExpr = JsExpr.App(JsExpr.Select(a, "isEqualTo"), List(b))
    override def neqv(a: JsExpr, b: JsExpr): JsExpr = JsExpr.PrefixOp("!", eqv(a, b))
  }
}
