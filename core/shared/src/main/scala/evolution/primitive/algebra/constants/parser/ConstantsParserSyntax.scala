package evolution.primitive.algebra.constants.parser

import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.parser.{ ByVarParser, ByVarParsers, ParserConfig, PrimitiveParsers }
import evolution.primitive.algebra.parser.ByVarParsers._
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.typeclass.VectorSpace

class ConstantsParserSyntax[S[_]](alg: Constants[S, Double]) extends Constants[ByVarParserK[S, ?], Unit] {
  override def double(d: Unit): ByVarParser[S[Double]] =
    ByVarParsers.doubleLiteral.map(alg.double)
  override def point(x: ByVarParser[S[Double]], y: ByVarParser[S[Double]]): ByVarParser[S[Point]] =
    function2("point", x, y).map { case (parsedX, parsedY) => alg.point(parsedX, parsedY) }
  override def add[T: VectorSpace](a: ByVarParser[S[T]], b: ByVarParser[S[T]]): ByVarParser[S[T]] =
    function2("add", a, b).map { case (parsedA, parsedB) => alg.add(parsedA, parsedB) }
  override def sin(d: ByVarParserK[S, Double]): ByVarParserK[S, Double] =
    function1("sin", d).map(alg.sin)
  override def cos(d: ByVarParserK[S, Double]): ByVarParserK[S, Double] =
    function1("cos", d).map(alg.cos)
}
