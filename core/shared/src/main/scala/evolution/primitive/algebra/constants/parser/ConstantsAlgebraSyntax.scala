package evolution.primitive.algebra.constants.parser

import cats.kernel.Semigroup
import cats.{Defer, MonoidK}
import evolution.geometry.Point
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.parser.ParserConfig
import evolution.primitive.algebra.parser.PrimitiveParsers.function2
import ParserConfig.White._
import fastparse.noApi._

class ConstantsAlgebraSyntax[S[_]](alg: ConstantsAlgebra[S]) extends ConstantsAlgebra[λ[α => Parser[S[α]]]] {
  override def double(d: Double): Parser[S[Double]] =
    P(d.toString).map(_ => alg.double(d))
  override def point(x: Double, y: Double): Parser[S[Point]] =
    function2("point", double(x), double(y)).map(_ => alg.point(x, y))
  override def add[T: Semigroup](a: Parser[S[T]], b: Parser[S[T]]): Parser[S[T]] =
    function2("add", a, b).map { case (parsedA, parsedB) => alg.add(parsedA, parsedB) }
}
