package evolution.drawing.algebra.parser

import evolution.drawing.algebra.DrawingAlgebra.{DoubleType, PointType}
import evolution.drawing.algebra.interpreter.Builder
import evolution.drawing.algebra.interpreter.Builder._
import evolution.drawing.algebra.{Drawing, DrawingAlgebra, DrawingOpen}
import evolution.geometry.Point
import fastparse.{WhitespaceApi, all, core}

trait DrawingParser[+A] {
  def parse(s: String): Either[String, Drawing[Unit, A]]
  // def parse[F[+_]](alg: DrawingAlgebra[F])(s: String): Either[String, F[A]]
}

object DrawingParser {
  type Error = String

  object Parsers {
    val White = WhitespaceApi.Wrapper{
      import fastparse.all._
      NoTrace(CharIn(" ", "\n", "\r").rep)
    }
    import fastparse.noApi._
    import White._

    val digit: Parser[Unit] =
      P(CharIn('0' to '9'))

    val floatDigits: all.Parser[Unit] =
      P(digit.rep ~ "." ~ digit.rep(1))

    val double: Parser[Double] =
      P("-".? ~ (floatDigits | digit.rep(1))).!.map(_.toDouble)

    val point: Parser[Point] =
      function2("point", double, double).map { case (x, y) => Point(x, y) }

    def literal[T: DrawingAlgebra.Type]: Parser[T] =
      DrawingAlgebra.typeInstance[T].foldT(double, point)

    def const[E, T: DrawingAlgebra.Type]: Parser[Drawing[E, T]] =
      literal[T].map(t => Builder.const[E, T](t))

    def cartesian[E]: Parser[Drawing[E, Point]] =
      function2("point", doubleDrawing, doubleDrawing).map { case (x, y) => Builder.point(x, y)}

    def polar[E]: Parser[Drawing[E, Point]] =
      function2("polar", doubleDrawing, doubleDrawing).map { case (x, y) => Builder.polar(x, y)}

    def rnd[E]: Parser[Drawing[E, Double]] =
      function2("rnd", double, double).map { case (x, y) => Builder.rnd(x, y) }

    def integrate[E, T: DrawingAlgebra.Type]: Parser[Drawing[E, T]] =
      function2("integrate", literal[T], drawing[E, T]).map { case (s, f) => Builder.integrate(s, f)}

    def derive[E, T: DrawingAlgebra.Type]: Parser[Drawing[E, T]] =
      function1("derive", drawing[E, T]).map { f => Builder.derive(f)}

    def doubleDrawing[E]: Parser[Drawing[E, Double]] =
      P(const[E, Double] | rnd | integrate[E, Double] | derive[E, Double])

    def pointDrawing[E]: Parser[Drawing[E, Point]] =
      P(cartesian | polar | const[E, Point] | integrate[E, Point] | derive[E, Point])

    def var0[E, A](varName: String): Parser[DrawingOpen[E, A, A]] =
      P("$" ~ varName).map(_ => Builder.var0)

//    def shift[E, A, B](current: Parser[Drawing[E, A]]): Parser[TermE[(Int, E), A]] =
//      current.map(t => BuilderE.varS(t))

    def drawing[E, T: DrawingAlgebra.Type]: Parser[Drawing[E, T]] =
      DrawingAlgebra.typeInstance[T].foldT[Lambda[A => Parser[Drawing[E, A]]]](
        doubleDrawing,
        pointDrawing
      )

    def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
      P(funcName ~ "(" ~ parser ~ ")")
    def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ ")")
    def function3[A, B, C](funcName: String, parser1: Parser[A], parser2: Parser[B], parser3: Parser[C]): Parser[(A, B, C)] =
      P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")
  }

  implicit object DoubleDrawingParser extends DrawingParser[Double] {
    override def parse(s: String): Either[String, Drawing[Unit, Double]] =
      toEither(Parsers.doubleDrawing.parse(s))
  }

  implicit object PointDrawingParser extends DrawingParser[Point] {
    override def parse(s: String): Either[String, Drawing[Unit, Point]] =
      toEither(Parsers.pointDrawing[Unit].parse(s))
  }

  def parse[T](s: String)(implicit parser: DrawingParser[T]): Either[String, Drawing[Unit, T]] =
    parser.parse(s)

  private def toEither[T, Elem, Repr](result: core.Parsed[T, Elem, Repr]): Either[String, T] = {
    result.fold((_, _, failure) => Left(failure.toString), (t, _) => Right(t))
  }
}
