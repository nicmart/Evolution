package evolution.primitive.algebra.constants.parser

import cats.kernel.Semigroup
import cats.{Defer, MonoidK}
import evolution.geometry.Point
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.parser.{ParserConfig, PrimitiveParsers}
import evolution.primitive.algebra.parser.PrimitiveParsers.function2
import ParserConfig.White._
import fastparse.noApi._

class ConstantsSyntax[S[_]](alg: Constants[S]) extends Constants[λ[α => Parser[S[α]]]] {
  override def double(d: Double): Parser[S[Double]] =
    P(d.toString).map(_ => alg.double(d))
  override def point(x: Parser[S[Double]], y: Parser[S[Double]]): Parser[S[Point]] =
    function2("point", x, y).map { case (parsedX, parsedY) => alg.point(parsedX, parsedY) }
  override def add[T: Semigroup](a: Parser[S[T]], b: Parser[S[T]]): Parser[S[T]] =
    function2("add", a, b).map { case (parsedA, parsedB) => alg.add(parsedA, parsedB) }

  val anyDouble: Parser[S[Double]] = PrimitiveParsers.doubleLiteral.map(alg.double)
  val anyPoint: Parser[S[Point]] = point(anyDouble, anyDouble)
}