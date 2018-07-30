package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.DrawingAlgebra
import evolution.primitive.algebra.parser.ExtensibleParser.HasParser

class DrawingAlgebraParser[S[_], F[_]](alg: DrawingAlgebra[S, F]) {
  private val coreDrawingAlgebraParser: CoreDrawingAlgebraParser[S, F] =
    new CoreDrawingAlgebraParser[S, F](alg.drawing)
  private val bindingAlgebraParserS: BindingAlgebraParser[S] =
    new BindingAlgebraParser[S](alg.bindS)
  private val bindingAlgebraParserF: BindingAlgebraParser[F] =
    new BindingAlgebraParser[F](alg.bindF)
  private val scalarAlgebraParser: ScalarAlgebraParser[S] =
    new ScalarAlgebraParser[S](alg.scalar)

  def buildContainer[C](container: C)(
    implicit
    doubleS: HasParser[C, S[Double]],
    doubleF: HasParser[C, F[Double]],
    pointS: HasParser[C, S[Point]],
    pointF: HasParser[C, F[Point]],
  ): C = {
    val withDrawings = coreDrawingAlgebraParser.buildContainer2[C, Double, Point](container)
    val withScalars = scalarAlgebraParser.buildContainer[C, Double](withDrawings)
    val withLetBindingsForF = bindingAlgebraParserF.buildContainer2[C, Double, Point](withScalars)
    val withLetBindingsForS = bindingAlgebraParserF.buildContainer2[C, Double, Point](withLetBindingsForF)
    withLetBindingsForS
  }
}

case class DrawingAlgebraParserContainer[S[_], F[_]](
  doubleParserS: ExtensibleParser[DrawingAlgebraParserContainer[S, F], S[Double]],
  doubleParserF: ExtensibleParser[DrawingAlgebraParserContainer[S, F], F[Double]],
  pointParserS: ExtensibleParser[DrawingAlgebraParserContainer[S, F], S[Point]],
  pointParserF: ExtensibleParser[DrawingAlgebraParserContainer[S, F], F[Point]]
)

object DrawingAlgebraParserContainer {
  implicit def ops[S[_], F[_]](
    container: DrawingAlgebraParserContainer[S, F]
  ): ParsersContainerOps[DrawingAlgebraParserContainer[S, F]] =
    new ParsersContainerOps[DrawingAlgebraParserContainer[S, F]](container)

  def empty[S[_], F[_]]: DrawingAlgebraParserContainer[S, F] =
    DrawingAlgebraParserContainer[S, F](
      ExtensibleParser.fail,
      ExtensibleParser.fail,
      ExtensibleParser.fail,
      ExtensibleParser.fail
    )
  implicit def hasDoubleFParser[S[_], F[_]]: HasParser[DrawingAlgebraParserContainer[S, F], F[Double]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F], F[Double]](_.doubleParserF, (c, p) => c.copy(doubleParserF = p))
  implicit def hasDoubleSParser[S[_], F[_]]: HasParser[DrawingAlgebraParserContainer[S, F], S[Double]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F], S[Double]](_.doubleParserS, (c, p) => c.copy(doubleParserS = p))
  implicit def hasPointFParser[S[_], F[_]]: HasParser[DrawingAlgebraParserContainer[S, F], F[Point]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F], F[Point]](_.pointParserF, (c, p) => c.copy(pointParserF = p))
  implicit def hasPointSParser[S[_], F[_]]: HasParser[DrawingAlgebraParserContainer[S, F], S[Point]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F], S[Point]](_.pointParserS, (c, p) => c.copy(pointParserS = p))
}
