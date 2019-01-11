package evolution.primitive.algebra.constants.parser

import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.primitive.algebra.constants.{ Constants, ConstantsSyntax }
import evolution.primitive.algebra.parser.{ ByVarParser, ByVarParsers, ParserConfig, PrimitiveParsers }
import evolution.primitive.algebra.parser.ByVarParsers._
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.typeclass.VectorSpace
import fastparse.noApi._

class ConstantsParserSyntax[S[_]](alg: Constants[S]) extends ConstantsSyntax[ByVarParserK[S, ?]] {
  override def allIntegers: ByVarParser[S[Int]] =
    ByVarParsers.intLiteral.map(alg.int)
  override def allDoubles: ByVarParser[S[Double]] =
    ByVarParsers.doubleLiteral.map(alg.double)
  override def double(d: Double): ByVarParser[S[Double]] =
    ByVarParser.Raw(ctx => P(d.toString)).map(_ => alg.double(d))
  override def int(n: Int): ByVarParserK[S, Int] =
    ByVarParser.Raw(ctx => P(n.toString)).map(_ => alg.int(n))
  override def point(x: ByVarParser[S[Double]], y: ByVarParser[S[Double]]): ByVarParser[S[Point]] =
    function2("point", x, y).map { case (parsedX, parsedY) => alg.point(parsedX, parsedY) }
  override def add[T: Semigroup](a: ByVarParser[S[T]], b: ByVarParser[S[T]]): ByVarParser[S[T]] =
    function2("add", a, b).map { case (parsedA, parsedB) => alg.add(parsedA, parsedB) }
  override def sin(d: ByVarParserK[S, Double]): ByVarParserK[S, Double] =
    function1("sin", d).map(alg.sin)
  override def cos(d: ByVarParserK[S, Double]): ByVarParserK[S, Double] =
    function1("cos", d).map(alg.cos)
  override def multiply[T: VectorSpace](
    kParser: ByVarParserK[S, Double],
    tParser: ByVarParserK[S, T]): ByVarParserK[S, T] =
    function2("multiply", kParser, tParser).map { case (k, t) => alg.multiply(k, t) }
  override def eq[T: Eq](aParser: ByVarParserK[S, T], bParser: ByVarParserK[S, T]): ByVarParserK[S, Boolean] =
    function2("eq", aParser, bParser).map { case (a, b) => alg.eq(a, b) }
  override def ifThen[T](
    conditionParser: ByVarParserK[S, Boolean],
    aParser: ByVarParserK[S, T],
    bParser: ByVarParserK[S, T]): ByVarParserK[S, T] =
    function3("if", conditionParser, aParser, bParser).map { case (condition, a, b) => alg.ifThen(condition, a, b) }
}
