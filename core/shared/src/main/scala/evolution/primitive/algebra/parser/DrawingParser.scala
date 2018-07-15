package evolution.primitive.algebra.parser

import evolution.primitive.algebra._
import _root_.evolution.geometry.Point
import fastparse.{WhitespaceApi, all, core}

trait DrawingParser[A] {
  def parse(s: String): Either[String, DrawingExpr[A]]
}

object DrawingParser {
  import DrawingParserImpl.initialParsers

  implicit object DoubleDrawingParser extends DrawingParser[Double] {
    override def parse(s: String): Either[String, DrawingExpr[Double]] =
      toEither(initialParsers.get[Double].parse(s))
  }

  implicit object PointDrawingParser extends DrawingParser[Point] {
    override def parse(s: String): Either[String, DrawingExpr[Point]] =
      toEither(initialParsers.get[Point].parse(s))
  }

  private def toEither[T, Elem, Repr](result: core.Parsed[T, Elem, Repr]): Either[String, T] = {
    result.fold((_, _, failure) => Left(failure.toString), (t, _) => Right(t))
  }
}
