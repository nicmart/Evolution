package evolution.drawing.algebra.parser

import evolution.drawing.algebra.interpreter.Builder
import evolution.drawing.algebra.{Drawing, DrawingAlgebra}
import evolution.geometry.Point
import fastparse.{all, core}
import fastparse.all._

trait DrawingParser[+A] {
  def parse(s: String): Either[String, Drawing[A]]
  // def parse[F[+_]](alg: DrawingAlgebra[F])(s: String): Either[String, F[A]]
}

object DrawingParser {
  type Error = String

  object Parsers {
    val digit: Parser[Unit] =
      P(CharIn('0' to '9'))
    val floatDigits: all.Parser[Unit] =
      P(digit.rep ~ "." ~ digit.rep(1))
    val double: Parser[Double] =
      P((floatDigits | digit.rep(1)).!.map(_.toDouble))
    val point: Parser[Point] =
      P("point(" ~ double ~ "," ~ double ~ ")").map { case (x, y) => Point(x, y) }
    val constPoint: Parser[Drawing[Point]] =
      P("point(" ~ double ~ "," ~ double ~ ")").map { case (x, y) => Builder.cartesian(Builder.const(x), Builder.const(y)) }

    val const: Parser[Drawing[Double]] =
      P(double | "const(" ~ double ~ ")").map(Builder.const)

    val cartesian: Parser[Drawing[Point]] =
      P("cartesian(" ~ doubleDrawing ~ "," ~ doubleDrawing ~ ")").map { case (x, y) => Builder.cartesian(x, y)}

    val polar: Parser[Drawing[Point]] =
      P("polar(" ~ doubleDrawing ~ "," ~ doubleDrawing ~ ")").map { case (x, y) => Builder.polar(x, y)}

    val rnd: Parser[Drawing[Double]] =
      P("rnd(" ~ double ~ "," ~ double ~ ")").map { case (x, y) => Builder.rnd(x, y) }

    val integrateDouble: Parser[Drawing[Double]] =
      P("integrateDouble(" ~ double ~ "," ~ doubleDrawing ~ ")").map { case (start, f) => Builder.integrateDouble(start, f) }

    val integratePoint: Parser[Drawing[Point]] =
      P("integratePoint(" ~ point ~ "," ~ pointDrawing ~ ")").map { case (start, f) => Builder.integratePoint(start, f) }

    val deriveDouble: Parser[Drawing[Double]] =
      P("deriveDouble(" ~ doubleDrawing ~ ")").map { Builder.deriveDouble }

    val derivePoint: Parser[Drawing[Point]] =
      P("derivePoint(" ~ pointDrawing ~ ")").map { Builder.derivePoint }

    lazy val doubleDrawing: Parser[Drawing[Double]] =
      P(const | rnd | integrateDouble | deriveDouble)

    lazy val pointDrawing: Parser[Drawing[Point]] =
      P(cartesian | polar | constPoint | integratePoint | derivePoint)
  }

  implicit object DoubleDrawingParser extends DrawingParser[Double] {
    override def parse(s: String): Either[String, Drawing[Double]] =
      toEither(Parsers.doubleDrawing.parse(s))
  }

  implicit object PointDrawingParser extends DrawingParser[Point] {
    override def parse(s: String): Either[String, Drawing[Point]] =
      toEither(Parsers.pointDrawing.parse(s))
  }

  def parse[T](s: String)(implicit parser: DrawingParser[T]): Either[String, Drawing[T]] =
    parser.parse(s)

  private def toEither[T, Elem, Repr](result: core.Parsed[T, Elem, Repr]): Either[String, T] =
    result.fold((_, _, _) => Left("error"), (t, _) => Right(t))
}
