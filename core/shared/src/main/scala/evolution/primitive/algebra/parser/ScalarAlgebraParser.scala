package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.ScalarAlgebra
import ParsersContainerOps._
import evolution.primitive.algebra.parser.DependentParser.HasParser

class ScalarAlgebraParser[S[_]](alg: ScalarAlgebra[S]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer[C, T](container: C)(
    implicit
    hasDouble: HasParser[C, S[Double]],
    hasPoint: HasParser[C, S[Point]]
  ): C =
    container
      .addParser(dependentDoubleParser)
      .addParser(dependentPointParser)
      .addParser(dependentAddParser)

  private def dependentDoubleParser[C]: DependentParser[C, S[Double]] =
    DependentParser(_ => double.map(alg.double))

  private def dependentPointParser[C]: DependentParser[C, S[Point]] =
    DependentParser(_ => function2("point", double, double).map { case (x, y) => alg.point(Point(x, y)) })

  private def dependentAddParser[C](implicit hasParser: HasParser[C, S[Point]]): DependentParser[C, S[Point]] =
    DependentParser(c => function2("add", c.parser[S[Point]], c.parser[S[Point]]).map { case (a, b) => alg.add(a, b) })
}

case class ScalarParserContainer[S[_]](
  doubleParser: DependentParser[ScalarParserContainer[S], S[Double]],
  pointParser: DependentParser[ScalarParserContainer[S], S[Point]]
)

object ScalarParserContainer {
  def empty[S[_]]: ScalarParserContainer[S] =
    ScalarParserContainer[S](DependentParser.empty, DependentParser.empty)
  implicit def hasDouble[S[_]]: HasParser[ScalarParserContainer[S], S[Double]] =
    HasParser.instance(_.doubleParser, (c, p) => c.copy(doubleParser = p))
  implicit def hasPoint[S[_]]: HasParser[ScalarParserContainer[S], S[Point]] =
    HasParser.instance(_.pointParser, (c, p) => c.copy(pointParser = p))
}
