package evolution.primitive.algebra.parser

import evolution.primitive.algebra._
import _root_.evolution.primitive.algebra.interpreter.Builder
import _root_.evolution.geometry.Point
import evolution.primitive.algebra.parser.DrawingParserImpl.StaticParsers.varName
import fastparse.{WhitespaceApi, all, core}

object DrawingParserImpl {
  type Error = String

  private object Config {
    import fastparse.all._
    val whitespaces = CharIn(" ", "\n", "\r").rep
    val White = WhitespaceApi.Wrapper {
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

    def varUsage(varName: String): Parser[String] = P("$" ~ varName.!)

    def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
      P(funcName ~ "(" ~ parser ~ ")")
    def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ ")")
    def function3[A, B, C](
      funcName: String,
      parser1: Parser[A],
      parser2: Parser[B],
      parser3: Parser[C]
    ): Parser[(A, B, C)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")
    def prefix[A](operator: String, parser: Parser[A]): Parser[A] =
      P(operator ~ parser)
    def infix[A, B](operator: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
      P(parser1 ~ operator ~ parser2)
    def whitespaceWrap[T](p: Parser[T]): Parser[T] =
      P(Config.whitespaces ~ p ~ Config.whitespaces)
  }

  case class Parsers[G[_]](double: Parser[G[Double]], point: Parser[G[Point]]) extends TypeAlg[λ[X => Parser[G[X]]]] {
    def get[T: Type]: Parser[G[T]] =
      Type[T].run[λ[X => Parser[G[X]]]](this)
  }

  import StaticParsers._

  class BindingAlgebraParser[F[_]](alg: BindingAlgebra[F], current: Parser[F]) {
    def var0[A](varName: String): Parser[F[A]] =
      P("$" ~ varName ~ !varName).map(_ => alg.var0)

    def shift[A]: Parser[F[A]]
  }

  class ParserModule[S[_], F[_]](alg: DrawingAlgebra[S, F]) {

    private class ByEnvParsers(scalarVariables: Parsers[S], evolutionVariables: Parsers[F]) {
      type P[A, B] = Parser[DrawingExpr[B]]
      type ParserOf[T] = Parser[DrawingExpr[T]]
      type ScalarParserOf[T] = Parser[ScalarExpr[T]]
      val b = Builder

      def withVar[In: Type](varname: String): ByEnvParsers =
        new ByEnvParsers(pushVar[In](varname, scalarVariables))
      private def pushVar[T: Type](varName: String, vars: Parsers): Parsers = {
        val pushedParsers: TypeAlg.Pair[P] = PairAlg[P](
          P(var0[Double](varName) | shift(vars.double)),
          shift(vars.point),
          shift(vars.double),
          P(var0[Point](varName) | shift(vars.point))
        )
        Parsers(TypesPair.get[T, Double].run[P](pushedParsers), TypesPair.get[T, Point].run[P](pushedParsers))
      }

      def scalar[T: Type]: Parser[S[T]] =
        literal[T].map(t => Type[T].fold(t)(alg.scalar.double, alg.scalar.point))

      def cons[T: Type]: Parser[F[T]] =
        function2("cons", scalarExpr.get[T], evolutionExpr.get[T]).map {
          case (head, tail) => alg.drawing.cons(head, tail)
        }

      def empty[T: Type]: Parser[F[T]] =
        P("nil").map(_ => alg.drawing.empty)

      def mapEmpty[T: Type]: Parser[F[T]] =
        function2("mapEmpty", evolutionExpr.get[T], evolutionExpr.get[T]).map {
          case (drawing1, drawing2) => alg.drawing.mapEmpty(drawing1)(drawing2)
        }

      def mapCons[T: Type, U: Type]: Parser[F[U]] =
        function2("mapCons", evolutionExpr.get[T], evolutionExpr.get[U]).map {
          case (drawing1, drawing2) => alg.drawing.mapCons(drawing1)(drawing2)
        }

      def var0[A](name: String): ParserOf[A] =
        P("$" ~ name ~ !varName).map(_ => b.var0)

      def shift[Out, In](current: ParserOf[Out]): ParserOf[Out] =
        current.map(t => b.shift(t))

      def let[In: Type, Out: Type](assignmentParser: Parser[(String, DrawingExpr[In])]): ParserOf[Out] =
        assignmentParser.flatMap {
          case (name, value) => whitespaceWrap(withVar[In](name).expr.get[Out].map(e => b.let(name, value)(e)))
        }

      def letFunc[In: Type, Out: Type]: ParserOf[Out] =
        P(let[In, Out](P("let" ~ "(" ~ varName ~ "," ~ expr.get[In] ~ "," ~ "")) ~ ")")

      def letInfix[In: Type, Out: Type]: ParserOf[Out] =
        let[In, Out](P(varName ~ "=" ~ expr.get[In]))

      def choose[Out: Type]: ParserOf[Out] =
        function3("choose", expr.get[Double], expr.get[Out], expr.get[Out]).map {
          case (probability, drawing1, drawing2) =>
            b.choose(probability, drawing1, drawing2)
        }

      def dist: ParserOf[Double] =
        function3("dist", expr.get[Double], expr.get[Double], expr.get[Double]).map {
          case (probability, drawing1, drawing2) =>
            b.dist(probability, drawing1, drawing2)
        }

      def polymorphicExpr[A: Type]: ParserOf[A] =
        P(
          scalarVariables.get[A]
            | const
            | derive
            | integrate
            | inverseFunc[A]
            | inversePrefix[A]
            | add[A]
            | mul[A]
            | slowDown[A]
            | choose[A]
            | letFunc[Double, A]
            | letFunc[Point, A]
            | letInfix[Double, A]
            | letInfix[Point, A]
        )

      def evolutionExpr: Parsers[F] = Parsers[F](
        whitespaceWrap(P(rnd | dist | polymorphicExpr[Double])),
        whitespaceWrap(P(cartesian | polar | polymorphicExpr[Point]))
      )

      def scalarExpr: Parsers[S] = Parsers[S](???, ???)
    }

    private def finalizeParsers(parsers: Parsers): Parsers =
      Parsers(P(Start ~ parsers.get[Double] ~ End), P(Start ~ parsers.get[Point] ~ End))

    val initialParsers: Parsers =
      finalizeParsers(new ByEnvParsers(Parsers(Fail, Fail)).expr)
  }
}
