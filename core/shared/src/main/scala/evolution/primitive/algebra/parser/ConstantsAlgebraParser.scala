package evolution.primitive.algebra.parser

import evolution.geometry.Point
import ParsersContainerOps._
import cats.kernel.Semigroup
import cats.instances.double._
import evolution.primitive.algebra.constants.ConstantsAlgebra

class ConstantsAlgebraParser[S[_]](alg: ConstantsAlgebra[S]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer[C, T](container: C)(
    implicit
    hasDouble: HasParser[C, S, Double],
    hasPoint: HasParser[C, S, Point]
  ): C =
    container
      .addParser[S, Double](dependentDoubleParser)
      .addParser[S, Point](dependentPointParser)
      .addParser[S, Double](dependentAddParser[C, Double])
      .addParser[S, Point](dependentAddParser[C, Point])

  private def dependentDoubleParser[C]: DependentParser[C, S[Double]] =
    DependentParser(_ => double.map(alg.double))

  private def dependentPointParser[C]: DependentParser[C, S[Point]] =
    DependentParser(_ => function2("point", double, double).map { case (x, y) => alg.point(x, y) })

  private def dependentAddParser[C, T: Semigroup](implicit hasParser: HasParser[C, S, T]): DependentParser[C, S[T]] =
    DependentParser(c => function2("add", c.parser[S, T], c.parser[S, T]).map { case (a, b) => alg.add(a, b) })
}

case class ScalarParserContainer[S[_]](
  doubleParser: DependentParser[ScalarParserContainer[S], S[Double]],
  pointParser: DependentParser[ScalarParserContainer[S], S[Point]]
)

object ScalarParserContainer {
  def empty[S[_]]: ScalarParserContainer[S] =
    ScalarParserContainer[S](DependentParser.empty, DependentParser.empty)
  implicit def hasDouble[S[_]]: HasParser[ScalarParserContainer[S], S, Double] =
    HasParser.instance(_.doubleParser, (c, p) => c.copy(doubleParser = p))
  implicit def hasPoint[S[_]]: HasParser[ScalarParserContainer[S], S, Point] =
    HasParser.instance(_.pointParser, (c, p) => c.copy(pointParser = p))
}
