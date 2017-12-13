package evolution.drawing.algebra.parser

import evolution.drawing.algebra.interpreter.Builder
import evolution.drawing.algebra.{Drawing, DrawingAlgebra}
import fastparse.{all, core}
import fastparse.all._

trait DrawingParser[+A] {
  def parse(s: String): Either[String, Drawing[A]]
  // def parse[F[+_]](alg: DrawingAlgebra[F])(s: String): Either[String, F[A]]
}

object DrawingParser {
  type Error = String

  object DoubleDrawingParser extends DrawingParser[Double] {
    val digit: Parser[Unit] =
      P(CharIn('0' to '9'))
    val floatDigits: all.Parser[Unit] =
      P(digit.rep ~ "." ~ digit.rep(1))
    val double: Parser[Double] =
      P((floatDigits | digit.rep(1)).!.map(_.toDouble))

    val const: Parser[Drawing[Double]] =
      P(double | "const(" ~ double ~ ")").map(Builder.const)
    val rnd: Parser[Drawing[Double]] =
      P("rnd(" ~ double ~ "," ~ double ~ ")").map { case (x, y) => Builder.rnd(x, y) }
    val integrateDouble: Parser[Drawing[Double]] =
      P("integrateDouble(" ~ double ~ "," ~ doubleDrawing ~ ")").map { case (start, f) => Builder.integrateDouble(start, f) }
    lazy val doubleDrawing: Parser[Drawing[Double]] =
      P(const | rnd | integrateDouble)

    override def parse(s: String): Either[String, Drawing[Double]] =
      toEither(doubleDrawing.parse(s))
  }

  def toEither[T, Elem, Repr](result: core.Parsed[T, Elem, Repr]): Either[String, T] =
    result.fold((_, _, _) => Left("error"), (t, _) => Right(t))
}
