package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.ScalarAlgebra
import evolution.primitive.algebra.parser.ExtensibleParser.HasParser
import ParsersContainerOps._

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
      .addExtensibleParser(extensibleParserDouble)
      .addExtensibleParser(extensibleParserPoint)

  private def extensibleParserDouble[C]: ExtensibleParser[C, S[Double]] =
    ExtensibleParser(double.map(alg.double), _ => Fail)

  private def extensibleParserPoint[C]: ExtensibleParser[C, S[Point]] =
    ExtensibleParser(function2("point", double, double).map { case (x, y) => alg.point(Point(x, y)) }, _ => Fail)
}

case class ScalarParserContainer[S[_]](
  doubleParser: ExtensibleParser[ScalarParserContainer[S], S[Double]],
  pointParser: ExtensibleParser[ScalarParserContainer[S], S[Point]]
)

object ScalarParserContainer {
  def empty[S[_]]: ScalarParserContainer[S] =
    ScalarParserContainer[S](ExtensibleParser.fail, ExtensibleParser.fail)
  implicit def hasDouble[S[_]]: HasParser[ScalarParserContainer[S], S[Double]] =
    HasParser.instance(_.doubleParser, (c, p) => c.copy(doubleParser = p))
  implicit def hasPoint[S[_]]: HasParser[ScalarParserContainer[S], S[Point]] =
    HasParser.instance(_.pointParser, (c, p) => c.copy(pointParser = p))
}
