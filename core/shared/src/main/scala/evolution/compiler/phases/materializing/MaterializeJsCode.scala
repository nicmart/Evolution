package evolution.compiler.phases.materializing

import evolution.data.Expr
import Expr._

// TODO this is an implementation
object MaterializeJsCode {

  def materialize[T](expr: Expr[T]): String =
    toJs(expr).js

  private def toJs[T](expr: Expr[T]): JsExpr = {
    expr match {
      case Dbl(d)     => JsExpr.Raw(d.toString)
      case Floor(d)   => JsExpr.App(JsExpr.Raw("Math.floor"), List(toJs(d)))
      case ToDbl(n)   => toJs(n) // There is only one numeric type in JS
      case Integer(n) => JsExpr.Raw(n.toString)
      case Pnt(x, y)  => JsExpr.Instance("Point", List(toJs(x), toJs(y)))

      case LiftedPnt(x, y) => zipIterable(toJs(x), toJs(y), (xx, yy) => JsExpr.Instance("Point", List(xx, yy)))

      case Var(name) => JsExpr.Raw(name)
      case Let(variable, value, expr) =>
        JsExpr.App(JsExpr.Lambda(List(variable), toJs(expr)), List(toJs(value)))

      case Expr.Constant(t) => JsExpr.Iterable(JsExpr.Raw(s"while(true) { yield ${toJs(t).js}; }"))

      case Uniform(from, to) =>
        JsExpr.Iterable(
          JsExpr.Raw(
            s"""while(true) {
              var min = ${toJs(from).js};
              yield Math.random() * (${toJs(to).js} - min) + min;
            }"""
          )
        )
    }
  }

  private sealed trait JsExpr {
    def js: String
  }

  private object JsExpr {
    case class Raw(code: String) extends JsExpr {
      def js: String = code.trim
    }
    case class Obj(fields: (String, JsExpr)*) extends JsExpr {
      def js: String =
        fields.map { case (key, jsExpr) => s""""$key": ${jsExpr.js}""" }.mkString("{", ", ", "}")
    }
    case class Lambda(args: List[String], body: JsExpr) extends JsExpr {
      def js: String = s"""
        function(${args.mkString(", ")}) {
          return ${body.js};
        }
      """.trim
    }
    case class Iterable(expr: JsExpr) extends JsExpr {
      def js: String = s"""
          {
            *[Symbol.iterator]() {
                ${expr.js}
            }
          }
        """.trim
    }

    case class Instance(className: String, args: List[JsExpr]) extends JsExpr {
      def js: String =
        s"new $className(${args.map(_.js).mkString(", ")})"
    }

    case class App(func: JsExpr, args: List[JsExpr]) extends JsExpr {
      def js: String =
        s"(${func.js})(${args.map(_.js).mkString(", ")})"
    }
  }

  private def zipIterable(a: JsExpr, b: JsExpr, f: (JsExpr, JsExpr) => JsExpr): JsExpr = JsExpr.Iterable(
    JsExpr.Raw(
      s"""
      var __it1 = ${a.js}[Symbol.iterator]();
      var __it2 = ${b.js}[Symbol.iterator]();

      var __a = __it1.next();
      var __b = __it2.next();

      while (!__a.done || !__b.done) {
        yield ${f(JsExpr.Raw("__a.value"), JsExpr.Raw("__a.value")).js};
        __a = __it1.next();
        __b = __it2.next();
      }
    """.trim
    )
  )
}
