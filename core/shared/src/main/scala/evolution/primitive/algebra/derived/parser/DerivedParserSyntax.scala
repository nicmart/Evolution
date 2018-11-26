package evolution.primitive.algebra.derived.parser
import evolution.geometry.Point
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._
import evolution.primitive.algebra.parser.ByVarParsers._
import evolution.typeclass.VectorSpace
import fastparse.noApi.{ P, Parser }

class DerivedParserSyntax[F[_], R[_]](alg: Derived[F, R]) extends Derived[F, ByVarParserK[R, ?]] {
  override def cartesian(
    parserX: ByVarParserK[R, F[Double]],
    parserY: ByVarParserK[R, F[Double]]): ByVarParserK[R, F[Point]] =
    function2("cartesian", parserX, parserY).map { case (x, y) => alg.cartesian(x, y) }

  override def constant[A](parserA: ByVarParserK[R, A]): ByVarParserK[R, F[A]] =
    function1("constant", parserA).map(a => alg.constant(a))

  override def polar(
    radiusParser: ByVarParserK[R, F[Double]],
    angleParser: ByVarParserK[R, F[Double]]): ByVarParserK[R, F[Point]] =
    function2("polar", radiusParser, angleParser).map { case (radius, angle) => alg.polar(radius, angle) }

  override def integrate[A: VectorSpace](
    startParser: ByVarParserK[R, A],
    speedParser: ByVarParserK[R, F[A]]): ByVarParserK[R, F[A]] =
    function2("integrate", startParser, speedParser).map { case (start, speed) => alg.integrate(start, speed) }
}
