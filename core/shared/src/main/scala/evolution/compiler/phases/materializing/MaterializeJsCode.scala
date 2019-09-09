package evolution.compiler.phases.materializing

import evolution.data.Expr
import Expr._

// TODO this is an implementation
object MaterializeJsCode {

  def materialize[T](expr: Expr[T]): String = {
    expr match {
      case Dbl(d)     => d.toString
      case Floor(d)   => s"Math.floor(${materialize(d)})"
      case ToDbl(n)   => materialize(n)
      case Integer(n) => n.toString
      case Pnt(x, y)  => JsObj("x" -> materialize(x), "y" -> materialize(y)).json

      case Var(name) => name
      case Let(variable, value, expr) =>
        JsApp(JsLambda(List(variable), materialize(expr)).js, List(materialize(value))).js
      case Expr.Constant(t) => ???
    }
  }

  private case class JsObj(fields: (String, String)*) {
    def json: String =
      fields.map { case (key, jsonValue) => s""""$key": $jsonValue""" }.mkString("{", ", ", "}")
  }

  private case class JsLambda(args: List[String], body: String) {
    def js: String =
      s"""
      function(${args.mkString(", ")}) {
        return $body;
      }
    """.stripMargin
  }

  private case class JsApp(func: String, args: List[String]) {
    def js: String =
      s"($func)(${args.mkString(", ")})"
  }
}
