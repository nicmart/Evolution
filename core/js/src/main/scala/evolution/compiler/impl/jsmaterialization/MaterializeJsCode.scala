package evolution.compiler.impl.jsmaterialization

import evolution.compiler.expression.Expr
import Expr._

// TODO this is an implementation
object MaterializeJsCode {

  def materialize[T](expr: Expr[T]): String =
    toJs(expr).js

  private def toJs[T](expr: Expr[T]): JsExpr = {
    expr match {
      case Dbl(d) => JsExpr.Raw(d.toString)

      case Floor(d) => JsExpr.App(JsExpr.Raw("Math.floor"), List(toJs(d)))

      case ToDouble(n) => toJs(n) // There is only one numeric type in JS

      case Integer(n) => JsExpr.Raw(n.toString)

      case Pnt(x, y) => JsExpr.Instance("Point", List(toJs(x), toJs(y)))

      case LiftedPnt(x, y) => zipIterable(toJs(x), toJs(y), (xx, yy) => JsExpr.Instance("Point", List(xx, yy)))

      case Polar(x, y) => polar(toJs(x), toJs(y))

      case LiftedPolar(r, alpha) => zipIterable(toJs(r), toJs(alpha), polar)

      case X(p) => JsExpr.Select(toJs(p), "x")

      case Y(p) => JsExpr.Select(toJs(p), "y")

      case Norm(p) => JsExpr.Select(toJs(p), "norm")

      case Versor(p) => JsExpr.Select(toJs(p), "versor")

      case Add(a, b, add) => MaterializeAddition(add)(toJs(a), toJs(b))

      case Div(a, b) => JsExpr.BinaryOp(toJs(a), "/", toJs(b))

      case Exp(a, b) => ???

      case Sign(a) => ???

      case Mod(a, b) => ???

      case Inverse(t, inv) => MaterializeInverse(inv)(toJs(t))

      case Multiply(a, b, mult) => MaterializeMultiplication(mult)(toJs(a), toJs(b))

      case Sin(d) => ???

      case Cos(d) => ???

      case Lst(ts) => ???

      case SmoothStep(from, to, position) => ???

      case Equals(a, b, eq) => MaterializeEquality(eq).eqv(toJs(a), toJs(b))

      case Neq(a, b, eq) => MaterializeEquality(eq).neqv(toJs(a), toJs(b))

      case IfThen(condition, a, b) => ???

      case Bool(b) => ???

      case And(a, b) => ???

      case Or(a, b) => ???

      case Not(a) => ???

      case LessThan(a, b, cmp) => MaterializeComparison(cmp).lt(toJs(a), toJs(b))

      case LessThanOrEqual(a, b, cmp) => MaterializeComparison(cmp).lteqv(toJs(a), toJs(b))

      case GreaterThan(a, b, cmp) => MaterializeComparison(cmp).gt(toJs(a), toJs(b))

      case GreaterThanOrEqual(a, b, cmp) => MaterializeComparison(cmp).gteqv(toJs(a), toJs(b))

      case InRect(topLeft, bottomDown, point) => ???

      case Var(name) => JsExpr.Raw(name)

      case Let(variable, value, expr) =>
        JsExpr.App(JsExpr.Lambda(List(variable), toJs(expr)), List(toJs(value)))

      case Lambda(name, body) => JsExpr.Lambda(List(name), toJs(body))

      case App(f, a) => JsExpr.App(toJs(f), List(toJs(a)))

      case Expr.Constant(t) => JsExpr.Iterable(JsExpr.Raw(s"while(true) { yield ${toJs(t).js}; }"))

      case Fix(expr) => ???

      case Empty() => ???

      case Cons(head, tail) => ???

      case Concat(as1, as2) => ???

      case MapEmpty(eva, eva2) => ???

      case MapCons(eva, f) => ???

      case ZipWith(fa, fb, f) => zipIterableApp(toJs(fa), toJs(fb), toJs(f))

      case Take(n, fa) => ???

      case TakeWhile(fa, predicate) => ???

      case WithFirst(as, f) => ???

      case FlatMap(fa, f) => ???

      case Flatten(ffa) => ???

      case Parallel(ffa) => ???

      case Map(fa, f) => mapIterable(toJs(fa), a => JsExpr.App(toJs(f), List(a)))

      case MapWithDerivative(fa, f, sg, inv) => ???

      case Range(from, to, step) => ???

      case Uniform(from, to) =>
        JsExpr.Iterable(
          JsExpr.Raw(
            s"""while(true) {
              var min = ${toJs(from).js};
              yield Math.random() * (${toJs(to).js} - min) + min;
            }"""
          )
        )

      case UniformChoice(choices) => ???

      case UniformDiscrete(from, to, step) => ???

      case UniformFrom(n, ft) => ???

      case Integrate(start, speed, semigroup) =>
        val adder = MaterializeAddition(semigroup) _
        JsExpr.Iterable(JsExpr.Raw(s"""
          var __it1 = ${toJs(speed).js}[Symbol.iterator]();
    
          var __current = ${toJs(start).js};

          yield __current;

          var __a = __it1.next();

          while (!__a.done) {
            __current = ${adder(JsExpr.Raw("__current"), JsExpr.Raw("__a.value")).js};
            yield __current;
            __a = __it1.next();
          }
      """.trim))

      case Solve1(speed, start, semigroup) => ???

      case Solve2(acc, a0, v0, semigroup) => ???

      case Derive(t, sg, inv) => ???

      case Normal(μ, σ) => ???

      case Noise() => ???

      case OctaveNoise() => ???
    }
  }

  sealed trait JsExpr {
    def js: String
  }

  object JsExpr {
    case class Raw(code: String) extends JsExpr {
      def js: String = code.trim
    }

    case class PrefixOp(op: String, expr: JsExpr) extends JsExpr {
      def js: String = s"${op}${expr.js})"
    }

    case class BinaryOp(left: JsExpr, op: String, right: JsExpr) extends JsExpr {
      def js: String = s"(${left.js} $op ${right.js})"
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

    case class AppCurried(func: JsExpr, args: List[JsExpr]) extends JsExpr {
      def js: String =
        s"(${func.js})${args.map(arg => s"(${arg.js})").mkString("")}"
    }

    case class Select(obj: JsExpr, field: String) extends JsExpr {
      def js: String = s"${obj.js}.$field"
    }
  }

  def polar(x: JsExpr, y: JsExpr): JsExpr = JsExpr.Instance(
    "Point",
    List(
      JsExpr.BinaryOp(x, "*", JsExpr.App(JsExpr.Raw("Math.cos"), List(y))),
      JsExpr.BinaryOp(x, "*", JsExpr.App(JsExpr.Raw("Math.sin"), List(y)))
    )
  )

  def mapIterable(fa: JsExpr, f: JsExpr => JsExpr): JsExpr = JsExpr.Iterable(
    JsExpr.Raw(
      s"""
      var __it1 = ${fa.js}[Symbol.iterator]();

      var __a = __it1.next();

      while (!__a.done) {
        yield ${f(JsExpr.Raw("__a.value")).js};
        __a = __it1.next();
      }
    """.trim
    )
  )

  def zipIterable(a: JsExpr, b: JsExpr, f: (JsExpr, JsExpr) => JsExpr): JsExpr = JsExpr.Iterable(
    JsExpr.Raw(
      s"""
      var __it1 = ${a.js}[Symbol.iterator]();
      var __it2 = ${b.js}[Symbol.iterator]();

      var __a = __it1.next();
      var __b = __it2.next();

      while (!__a.done || !__b.done) {
        yield ${f(JsExpr.Raw("__a.value"), JsExpr.Raw("__b.value")).js};
        __a = __it1.next();
        __b = __it2.next();
      }
    """.trim
    )
  )

  def zipIterableApp(a: JsExpr, b: JsExpr, f: JsExpr): JsExpr =
    zipIterable(a, b, (aa, bb) => JsExpr.AppCurried(f, List(aa, bb)))
}
