package evolution.drawing.algebra.parser

import evolution.drawing.algebra.DrawingAlgebra.{DoubleType, PointType}
import evolution.drawing.algebra.interpreter.Builder
import evolution.drawing.algebra._
import evolution.geometry.Point
import fastparse.{WhitespaceApi, all, core}

trait DrawingParser[+A] {
  def parse(s: String): Either[String, DrawingE[Empty, A]]
  // def parse[F[+_]](alg: DrawingAlgebra[F])(s: String): Either[String, F[A]]
}

object DrawingParser {
  type Error = String

//  object Parsers {
//    val White = WhitespaceApi.Wrapper{
//      import fastparse.all._
//      NoTrace(CharIn(" ", "\n", "\r").rep)
//    }
//    import fastparse.noApi._
//    import White._
//
//    case class CurrentParsers[E[_[_, _]]](double: Parser[DrawingE[E, Double]], point: Parser[DrawingE[E, Point]]) {
//      def shiftParsers[B](varName: String): CurrentParsers[Lambda[F[_, _] => SuccE[E, B, F]]] = ???
//      def parser[T: DrawingAlgebra.Type]: Parser[DrawingE[E, T]] =
//        DrawingAlgebra.typeInstance[T].foldT[Parser[DrawingE[E, ?]]](
//          double,
//          point
//        )
//    }
//
//    val digit: Parser[Unit] =
//      P(CharIn('0' to '9'))
//
//    val floatDigits: all.Parser[Unit] =
//      P(digit.rep ~ "." ~ digit.rep(1))
//
//    val double: Parser[Double] =
//      P("-".? ~ (floatDigits | digit.rep(1))).!.map(_.toDouble)
//
//    val point: Parser[Point] =
//      function2("point", double, double).map { case (x, y) => Point(x, y) }
//
//    def literal[T: DrawingAlgebra.Type]: Parser[T] =
//      DrawingAlgebra.typeInstance[T].foldT(double, point)
//
//    val varName: Parser[String] =
//      P(CharsWhileIn('a' to 'z').!)
//
//    def const[E[_[_, _]], T: DrawingAlgebra.Type]: Parser[DrawingE[E, T]] =
//      literal[T].map(t => Builder.const[E, T](t))
//
//    def cartesian[E[_[_, _]]]: Parser[DrawingE[E, Point]] =
//      function2("point", doubleDrawing[E], doubleDrawing[E]).map { case (x, y) => Builder.point[E](x, y)}
//
//    def polar[E[_[_, _]]]: Parser[DrawingE[E, Point]] =
//      function2("polar", doubleDrawing[E], doubleDrawing[E]).map { case (x, y) => Builder.polar(x, y)}
//
//    def rnd[E[_[_, _]]]: Parser[DrawingE[E, Double]] =
//      function2("rnd", double, double).map { case (x, y) => Builder.rnd(x, y) }
//
//    def integrate[E[_[_, _]], T: DrawingAlgebra.Type]: Parser[DrawingE[E, T]] =
//      function2("integrate", literal[T], drawing[E, T]).map { case (s, f) => Builder.integrate(s, f)}
//
//    def derive[E[_[_, _]], T: DrawingAlgebra.Type]: Parser[DrawingE[E, T]] =
//      function1("derive", drawing[E, T]).map { f => Builder.derive(f)}
//
//    def var0[E[_[_, _]], A](varName: String): Parser[DrawingS[E, A, A]] =
//      P("$" ~ varName).map(_ => Builder.var0)
//
//    def shift[E[_[_, _]], A, B](current: Parser[DrawingE[E, A]]): Parser[DrawingS[E, A, B]] =
//      current.map(t => Builder.shift(t))
//
//    def let[E[_[_, _]], A: DrawingAlgebra.Type, B: DrawingAlgebra.Type]
//      (current: CurrentParsers[E]): Parser[DrawingS[E, A, B]] =
//      P("let" ~/ "(" ~ varName ~/ "," ~ current.parser[A] ~/ "," ~ "").flatMap { case (name, value) =>
//        exprS(name, parser).map(e => Builder.let(name, value)(e))
//      } ~ ")"
//
//    def succFoo[E[_[_, _]], A, B](curr: Parser[Drawing[E, A]]): Parser[DrawingS[E, A, B]]
//
//    def parsers[E[_[_, _]]](current: => CurrentParsers[E]): CurrentParsers[E] = CurrentParsers[E](
//      P(const[E, Double] | rnd | integrate[E, Double] | derive[E, Double]),
//      P(cartesian[E] | polar[E] | const[E, Point] | integrate[E, Point] | derive[E, Point])
//    )
//
//    def parsersS[E[_[_, _]], A: DrawingAlgebra.Type, B: DrawingAlgebra.Type]
//      (varname: String, current: => CurrentParsers[E]): CurrentParsers[Lambda[F[_, _] => (F[E[F], A], E[F])]] =
//        CurrentParsers[Lambda[F[_, _] => (F[E[F], A], E[F])]](
//          P(const[E, Double] | rnd | integrate[E, Double] | derive[E, Double]),
//          P(cartesian[E] | polar[E] | const[E, Point] | integrate[E, Point] | derive[E, Point])
//        )
//
//    // TODO Here the type is WRONG
//    def varDouble[E[_[_, _]]](varname: String, current: => CurrentParsers[E]): CurrentParsers[E] =
//      parsers(parsersS(varname, current))
//
//    def initialParsers: CurrentParsers[Empty] = parsers(initialParsers)
//
//    def drawing[E[_[_, _]], T: DrawingAlgebra.Type](current: CurrentParsers[E]): Parser[DrawingE[E, T]] =
//      DrawingAlgebra.typeInstance[T].foldT[Lambda[A => Parser[DrawingE[E, A]]]](
//        current.double,
//        current.point
//      )
//
//    def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
//      P(funcName ~ "(" ~ parser ~ ")")
//    def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
//      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ ")")
//    def function3[A, B, C](funcName: String, parser1: Parser[A], parser2: Parser[B], parser3: Parser[C]): Parser[(A, B, C)] =
//      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")
//  }

  implicit object DoubleDrawingParser extends DrawingParser[Double] {
    override def parse(s: String): Either[String, DrawingE[Empty, Double]] =
      ???
      //toEither(Parsers.doubleDrawing.parse(s))
  }

  implicit object PointDrawingParser extends DrawingParser[Point] {
    override def parse(s: String): Either[String, DrawingE[Empty, Point]] =
      ???
      //toEither(Parsers.pointDrawing.parse(s))
  }

  def parse[T](s: String)(implicit parser: DrawingParser[T]): Either[String, DrawingE[Empty, T]] =
    parser.parse(s)

  private def toEither[T, Elem, Repr](result: core.Parsed[T, Elem, Repr]): Either[String, T] = {
    result.fold((_, _, failure) => Left(failure.toString), (t, _) => Right(t))
  }
}
