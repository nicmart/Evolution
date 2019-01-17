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
    function2("point", parserX, parserY).map { case (x, y) => alg.cartesian(x, y) }

  override def constant[A](parserA: ByVarParserK[R, A]): ByVarParserK[R, F[A]] =
    function1("constant", parserA).map(a => alg.constant(a))

  override def constantF[A](parserA: ByVarParserK[R, A]): ByVarParserK[R, F[A]] =
    parserA.map(a => alg.constant(a))

  override def polar(
    radiusParser: ByVarParserK[R, F[Double]],
    angleParser: ByVarParserK[R, F[Double]]): ByVarParserK[R, F[Point]] =
    function2("polar", radiusParser, angleParser).map { case (radius, angle) => alg.polar(radius, angle) }

  override def integrate[A: VectorSpace](
    startParser: ByVarParserK[R, A],
    speedParser: ByVarParserK[R, F[A]]): ByVarParserK[R, F[A]] =
    function2("integrate", startParser, speedParser).map { case (start, speed) => alg.integrate(start, speed) }

  override def solve1[X: VectorSpace](
    equationParser: ByVarParserK[R, F[X => X]],
    x0Parser: ByVarParserK[R, X]): ByVarParserK[R, F[X]] =
    function2("solve1", equationParser, x0Parser).map { case (equation, x0) => alg.solve1(equation, x0) }

  override def solve2[X: VectorSpace](
    eqParser: ByVarParserK[R, F[X => X => X]],
    x0Parser: ByVarParserK[R, X],
    v0Parser: ByVarParserK[R, X]): ByVarParserK[R, F[X]] =
    function3("solve2", eqParser, x0Parser, v0Parser).map { case (eq, x0, v0) => alg.solve2(eq, x0, v0) }

  override def map[A, B](faParser: ByVarParserK[R, F[A]], fParser: ByVarParserK[R, A => B]): ByVarParserK[R, F[B]] =
    function2("map", faParser, fParser).map { case (fa, f) => alg.map(fa, f) }

  override def concat[A](fa1Parser: ByVarParserK[R, F[A]], fa2Parser: ByVarParserK[R, F[A]]): ByVarParserK[R, F[A]] =
    function2("concat", fa1Parser, fa2Parser).map { case (fa1, fa2) => alg.concat(fa1, fa2) }

  override def flatMap[A, B](
    faParser: ByVarParserK[R, F[A]],
    fParser: ByVarParserK[R, A => F[B]]): ByVarParserK[R, F[B]] =
    function2("flatMap", faParser, fParser).map { case (fa, f) => alg.flatMap(fa, f) }

  override def take[T](nParser: ByVarParserK[R, Int], ftParser: ByVarParserK[R, F[T]]): ByVarParserK[R, F[T]] =
    function2("take", nParser, ftParser).map { case (n, ft) => alg.take(n, ft) }
}
