package evolution.drawing.algebra.parser

import evolution.drawing.algebra._
import evolution.drawing.algebra.interpreter.Builder
import evolution.geometry.Point
import fastparse.{WhitespaceApi, all, core}

trait DrawingParser[+A] {
  def parse(s: String): Either[String, DrawingExpr[Empty, A]]
}

object DrawingParser {
  type Error = String

  object Parsers {
    object Config {
      import fastparse.all._
      val whitespaces = CharIn(" ", "\n", "\r").rep
    }
    val White = WhitespaceApi.Wrapper{
      import fastparse.all._
      NoTrace(Config.whitespaces)
    }
    import White._
    import fastparse.noApi._

    def whitespaceWrap[T](p: Parser[T]): Parser[T] =
      P(Config.whitespaces ~ p ~ Config.whitespaces)

    case class Parsers[E[_[_, _]]](
      double: Parser[DrawingExpr[E, Double]],
      point: Parser[DrawingExpr[E, Point]]
    ) extends TypeAlg[λ[X => Parser[DrawingExpr[E, X]]]] {
      type P[A, B] = Parser[ExprS[E, B, A]]
      def pushVar[T: Type](varName: String): Parsers[λ[F[_, _] => (F[E[F], T], E[F])]] = {
        val pushedParsers: TypeAlg.Pair[P] = PairAlg[P](
          P(var0[E, Double](varName) | shift[E, Double, Double](double)),
          shift[E, Point, Double](point),
          shift[E, Double, Point](double),
          P(var0[E, Point](varName) | shift[E, Point, Point](point))
        )
        Parsers[λ[F[_, _] => (F[E[F], T], E[F])]](
          TypesPair.get[T, Double].run[P](pushedParsers),
          TypesPair.get[T, Point].run[P](pushedParsers)
        )
      }

      def get[T: Type]: Parser[DrawingExpr[E, T]] =
        Type[T].run[λ[X => Parser[DrawingExpr[E, X]]]](this)
    }

    object Parsers {
      def empty[E[_[_, _]]]: Parsers[E] = Parsers(Fail, Fail)
    }

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

    val varName: Parser[String] =
      P(CharsWhileIn('a' to 'z').!)

    def const[E[_[_, _]], T: Type]: Parser[DrawingExpr[E, T]] =
      literal[T].map(t => Builder.const[E, T](t))

    def cartesian[E[_[_, _]]](vars: Parsers[E]): Parser[DrawingExpr[E, Point]] =
      function2("point", expr(vars).get[Double], expr(vars).get[Double]).map {
        case (x, y) => Builder.point[E](x, y)
      }

    def polar[E[_[_, _]]](vars: Parsers[E]): Parser[DrawingExpr[E, Point]] =
      function2("polar", expr(vars).get[Double], expr(vars).get[Double]).map {
        case (x, y) => Builder.polar(x, y)
      }

    def rnd[E[_[_, _]]]: Parser[DrawingExpr[E, Double]] =
      function2("rnd", double, double).map { case (x, y) => Builder.rnd(x, y) }

    def integrate[E[_[_, _]], T: Type](vars: Parsers[E]): Parser[DrawingExpr[E, T]] =
      function2("integrate", literal[T], expr(vars).get[T]).map {
        case (s, f) => Builder.integrate(s, f)
      }

    def derive[E[_[_, _]], T: Type](vars: Parsers[E]): Parser[DrawingExpr[E, T]] =
      function1("derive", expr(vars).get[T]).map { f => Builder.derive(f) }

    def var0[E[_[_, _]], A](varName: String): Parser[ExprS[E, A, A]] =
      P("$" ~ varName).map(_ => Builder.var0[E, A])

    def shift[E[_[_, _]], Out, In](current: Parser[DrawingExpr[E, Out]]): Parser[ExprS[E, Out, In]] =
      current.map(t => Builder.shift[E, Out, In](t))

    def let[E[_[_, _]], In: Type, Out: Type](vars: Parsers[E]): Parser[DrawingExpr[E, Out]] =
      P(P("let" ~ "(" ~ varName ~ "," ~ expr(vars).get[In] ~ "," ~ "").flatMap {
        case (name, value) => whitespaceWrap(
            expr[λ[F[_, _] => (F[E[F], In], E[F])]](vars.pushVar[In](name)).get[Out].map(e => Builder.let[E, In, Out](name, value)(e))
          )
        } ~ ")")

    def polymorphicExpr[E[_[_, _]], A: Type](vars: => Parsers[E]): Parser[DrawingExpr[E, A]] =
      P(vars.get[A]
        | const[E, A]
        | derive[E, A](vars)
        | let[E, Double, A](vars)
        | integrate[E, A](vars)
        | let[E, Point, A](vars)
      )

    def expr[E[_[_, _]]](vars: Parsers[E]): Parsers[E] = Parsers[E](
      whitespaceWrap(P(rnd[E] | polymorphicExpr[E, Double](vars))),
      whitespaceWrap(P(cartesian[E](vars) | polar[E](vars) | polymorphicExpr[E, Point](vars)))
    )

    val initialParsers: Parsers[Empty] = expr(Parsers.empty[Empty])

    def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
      P(funcName ~ "(" ~ parser ~ ")")
    def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ ")")
    def function3[A, B, C](funcName: String, parser1: Parser[A], parser2: Parser[B], parser3: Parser[C]): Parser[(A, B, C)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")
  }

  implicit object DoubleDrawingParser extends DrawingParser[Double] {
    override def parse(s: String): Either[String, DrawingExpr[Empty, Double]] =
      toEither(Parsers.initialParsers.get[Double].parse(s))
  }

  implicit object PointDrawingParser extends DrawingParser[Point] {
    override def parse(s: String): Either[String, DrawingExpr[Empty, Point]] =
      toEither(Parsers.initialParsers.get[Point].parse(s))
  }

  def parse[T](s: String)(implicit parser: DrawingParser[T]): Either[String, DrawingExpr[Empty, T]] =
    parser.parse(s)

  private def toEither[T, Elem, Repr](result: core.Parsed[T, Elem, Repr]): Either[String, T] = {
    result.fold((_, _, failure) => Left(failure.toString), (t, _) => Right(t))
  }
}
