package evolution.drawing.algebra.parser

import evolution.drawing.algebra._
import evolution.drawing.algebra.interpreter.Builder
import evolution.geometry.Point
import fastparse.{WhitespaceApi, all, core}

object DrawingParserImpl {
  type Error = String

  private object Config {
    import fastparse.all._
    val whitespaces = CharIn(" ", "\n", "\r").rep
    val White = WhitespaceApi.Wrapper{
      NoTrace(Config.whitespaces)
    }
  }

  import Config.White._
  import fastparse.noApi._

  private object StaticParsers {
    val digit: Parser[Unit] =
      P(CharIn('0' to '9'))

    val floatDigits: all.Parser[Unit] =
      P(digit.rep ~ "." ~ digit.rep(1))

    val double: Parser[Double] =
      P("-".? ~ (floatDigits | digit.rep(1))).!.map(_.toDouble)

    val point: Parser[Point] =
      function2("point", double, double).map { case (x, y) => Point(x, y) }

    def literal[T: Type]: Parser[T] =
      Type[T].run(TypeAlg(double, point))

    val varName: Parser[String] = {
      val letter = P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn(Seq('_', '-')) | CharIn('0' to '9'))
      P(letter.rep(1).!)
    }

    def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
      P(funcName ~ "(" ~ parser ~ ")")
    def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ ")")
    def function3[A, B, C](funcName: String, parser1: Parser[A], parser2: Parser[B], parser3: Parser[C]): Parser[(A, B, C)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")
    def prefix[A](operator: String, parser: Parser[A]): Parser[A] =
      P(operator ~ parser)
    def infix[A, B](operator: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
      P(parser1 ~ operator ~ parser2)
    def whitespaceWrap[T](p: Parser[T]): Parser[T] =
      P(Config.whitespaces ~ p ~ Config.whitespaces)
  }

  case class Parsers[E[_[_, _]]](
    double: Parser[DrawingExpr[E, Double]],
    point: Parser[DrawingExpr[E, Point]]
  ) extends TypeAlg[λ[X => Parser[DrawingExpr[E, X]]]] {
    def get[T: Type]: Parser[DrawingExpr[E, T]] =
      Type[T].run[λ[X => Parser[DrawingExpr[E, X]]]](this)
  }

  import StaticParsers._

  private class ByEnvParsers[E[_[_, _]]](vars: Parsers[E]) {
    type P[A, B] = Parser[ExprS[E, B, A]]
    type ParserOf[T] = Parser[DrawingExpr[E, T]]
    type NextDrawingExpr[In, Out] = DrawingExpr[EnvS[?[_, _], E, In], Out]
    type NextParserOf[In, Out] = Parser[NextDrawingExpr[In, Out]]
    val b = new Builder[E]

    def withVar[In: Type](varname: String): ByEnvParsers[EnvS[?[_, _], E, In]] =
      new ByEnvParsers[EnvS[?[_, _], E, In]](pushVar[In](varname, vars))
    private def pushVar[T: Type](varName: String, vars: Parsers[E]): Parsers[EnvS[?[_, _], E, T]] = {
      val pushedParsers: TypeAlg.Pair[P] = PairAlg[P](
        P(var0[Double](varName) | shift(vars.double)),
        shift(vars.point),
        shift(vars.double),
        P(var0[Point](varName) | shift(vars.point))
      )
      Parsers[EnvS[?[_, _], E, T]](
        TypesPair.get[T, Double].run[P](pushedParsers),
        TypesPair.get[T, Point].run[P](pushedParsers)
      )
    }

    def const[T: Type]: ParserOf[T] =
      literal[T].map(t => b.const(t))

    def cartesian: ParserOf[Point] =
      function2("point", expr.get[Double], expr.get[Double]).map {
        case (x, y) => b.point(x, y)
      }

    def polar: ParserOf[Point] =
      function2("polar", expr.get[Double], expr.get[Double]).map {
        case (x, y) => b.polar(x, y)
      }

    def rnd: ParserOf[Double] =
      function2("rnd", expr.get[Double], expr.get[Double]).map { case (x, y) => b.rnd(x, y) }

    def add[T: Type]: ParserOf[T] =
      function2("add", expr.get[T], expr.get[T]).map { case (x, y) => b.add(x, y) }

    def inverseFunc[T: Type]: ParserOf[T] =
      function1("inverse", expr.get[T]).map { x => b.inverse(x) }

    def inversePrefix[T: Type]: ParserOf[T] =
      prefix("-", expr.get[T]).map { x => b.inverse(x) }

    def mul[T: Type]: ParserOf[T] =
      function2("mul", expr.get[Double], expr.get[T]).map { case (x, y) => b.mul(x, y) }

    def integrate[T: Type]: ParserOf[T] =
      function2("integrate", literal[T], expr.get[T]).map {
        case (s, f) => b.integrate(s, f)
      }

    def derive[T: Type]: ParserOf[T] =
      function1("derive", expr.get[T]).map { f => b.derive(f) }

    def slowDown[T: Type]: ParserOf[T] =
      function2("slowDown", expr.get[Double], expr.get[T]).map { case (x, y) => b.slowDown(x, y) }

    def var0[A](varName: String): NextParserOf[A, A] =
      P("$" ~ varName).map(_ => b.var0)

    def shift[Out, In](current: ParserOf[Out]): NextParserOf[In, Out] =
      current.map(t => b.shift(t))

    def let[In: Type, Out: Type](assignmentParser: Parser[(String, DrawingExpr[E, In])]): ParserOf[Out] =
      assignmentParser.flatMap {
        case (name, value) => whitespaceWrap(
          withVar[In](name).expr.get[Out].map(e => b.let(name, value)(_ => e))
        )
      }

    def letFunc[In: Type, Out: Type]: ParserOf[Out] =
      P(let[In, Out](P("let" ~ "(" ~ varName ~ "," ~ expr.get[In] ~ "," ~ "")) ~ ")")

    def letInfix[In: Type, Out: Type]: ParserOf[Out] =
      let[In, Out](P(varName ~ "=" ~ expr.get[In]))

    def polymorphicExpr[A: Type]: ParserOf[A] =
      P(vars.get[A]
        | const
        | derive
        | integrate
        | inverseFunc[A]
        | inversePrefix[A]
        | add[A]
        | mul[A]
        | slowDown[A]
        | letFunc[Double, A]
        | letFunc[Point, A]
        | letInfix[Double, A]
        | letInfix[Point, A]
      )

    def expr: Parsers[E] = Parsers[E](
      whitespaceWrap(P(rnd | polymorphicExpr[Double])),
      whitespaceWrap(P(cartesian | polar | polymorphicExpr[Point]))
    )
  }

  private def finalizeParsers(parsers: Parsers[Empty]): Parsers[Empty] =
    Parsers[Empty](
      P(Start ~ parsers.get[Double] ~ End),
      P(Start ~ parsers.get[Point] ~ End)
    )

  val initialParsers: Parsers[Empty] =
    finalizeParsers(new ByEnvParsers[Empty](Parsers(Fail, Fail)).expr)
}