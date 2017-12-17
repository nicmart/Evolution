package evolution.drawing.algebra.parser

import evolution.drawing.algebra.DrawingAlgebra.{DoubleType, PointType}
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
    def literal[T: DrawingAlgebra.Type]: Parser[T] =
      DrawingAlgebra.typeInstance[T].foldT(double, point)

    def const[T: DrawingAlgebra.Type]: Parser[Drawing[T]] =
      literal[T].map(t => Builder.const[T](t))

    val cartesian: Parser[Drawing[Point]] =
      P("cartesian(" ~ doubleDrawing ~ "," ~ doubleDrawing ~ ")").map { case (x, y) => Builder.cartesian(x, y)}

    val polar: Parser[Drawing[Point]] =
      P("polar(" ~ doubleDrawing ~ "," ~ doubleDrawing ~ ")").map { case (x, y) => Builder.polar(x, y)}

    val rnd: Parser[Drawing[Double]] =
      P("rnd(" ~ double ~ "," ~ double ~ ")").map { case (x, y) => Builder.rnd(x, y) }

    def integrate[T: DrawingAlgebra.Type]: Parser[Drawing[T]] =
      P("integrate(" ~ literal[T] ~ "," ~ drawing[T] ~ ")").map { case (s, f) => Builder.integrate(s, f)}

    def derive[T: DrawingAlgebra.Type]: Parser[Drawing[T]] =
      P("derive(" ~ drawing[T] ~ ")").map { f => Builder.derive(f)}

    lazy val doubleDrawing: Parser[Drawing[Double]] =
      P(const[Double] | rnd | integrate[Double] | derive[Double])

    lazy val pointDrawing: Parser[Drawing[Point]] =
      P(cartesian | polar | const[Point] | integrate[Point] | derive[Point])

    def drawing[T: DrawingAlgebra.Type]: Parser[Drawing[T]] =
      DrawingAlgebra.typeInstance[T].foldT[Lambda[A => Parser[Drawing[A]]]](
        doubleDrawing,
        pointDrawing
      )
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
