package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.DrawingAlgebra
import evolution.primitive.algebra.parser.ExtensibleParser.HasParser

class DrawingAlgebraParser[S[_], F[_], R[_]](alg: DrawingAlgebra[S, F, R]) {
  type RS[A] = R[S[A]]
  type RF[A] = R[F[A]]
  private val coreDrawingAlgebraParser: CoreDrawingAlgebraParser[RS, RF] =
    new CoreDrawingAlgebraParser[RS, RF](alg.drawing)
  private val bindingAlgebraParser: BindingAlgebraParser[R] =
    new BindingAlgebraParser[R](alg.bind)
  private val scalarAlgebraParser: ScalarAlgebraParser[RS] =
    new ScalarAlgebraParser[RS](alg.scalar)

  def buildContainer[C](container: C)(
    implicit
    doubleS: HasParser[C, RS[Double]],
    doubleF: HasParser[C, RF[Double]],
    pointS: HasParser[C, RS[Point]],
    pointF: HasParser[C, RF[Point]],
    hasShift: HasShift[C, R]
  ): C = {
    val withDrawings = coreDrawingAlgebraParser.buildContainer2[C, Double, Point](container)
    val withScalars = scalarAlgebraParser.buildContainer[C, Double](withDrawings)
    val withLetBindings = bindingAlgebraParser.buildContainer4[C, F[Double], F[Point], S[Double], S[Point]](withScalars)
    withLetBindings
  }
}

case class DrawingAlgebraParserContainer[S[_], F[_], R[_]](
  doubleParserS: ExtensibleParser[DrawingAlgebraParserContainer[S, F, R], R[S[Double]]],
  doubleParserF: ExtensibleParser[DrawingAlgebraParserContainer[S, F, R], R[F[Double]]],
  pointParserS: ExtensibleParser[DrawingAlgebraParserContainer[S, F, R], R[S[Point]]],
  pointParserF: ExtensibleParser[DrawingAlgebraParserContainer[S, F, R], R[F[Point]]]
)

object DrawingAlgebraParserContainer {
  import ParserConfig.White._
  import fastparse.noApi._

  implicit def ops[S[_], F[_], R[_]](
    container: DrawingAlgebraParserContainer[S, F, R]
  ): ParsersContainerOps[DrawingAlgebraParserContainer[S, F, R]] =
    new ParsersContainerOps[DrawingAlgebraParserContainer[S, F, R]](container)

  def empty[S[_], F[_], R[_]]: DrawingAlgebraParserContainer[S, F, R] =
    DrawingAlgebraParserContainer[S, F, R](
      ExtensibleParser.Empty(),
      ExtensibleParser.Empty(),
      ExtensibleParser.Empty(),
      ExtensibleParser.Empty()
    )
  implicit def hasDoubleFParser[S[_], F[_], R[_]]: HasParser[DrawingAlgebraParserContainer[S, F, R], R[F[Double]]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F, R], R[F[Double]]](
        _.doubleParserF,
        (c, p) => c.copy(doubleParserF = p)
      )

  implicit def hasDoubleSParser[S[_], F[_], R[_]]: HasParser[DrawingAlgebraParserContainer[S, F, R], R[S[Double]]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F, R], R[S[Double]]](
        _.doubleParserS,
        (c, p) => c.copy(doubleParserS = p)
      )

  implicit def hasPointFParser[S[_], F[_], R[_]]: HasParser[DrawingAlgebraParserContainer[S, F, R], R[F[Point]]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F, R], R[F[Point]]](_.pointParserF, (c, p) => c.copy(pointParserF = p))

  implicit def hasPointSParser[S[_], F[_], R[_]]: HasParser[DrawingAlgebraParserContainer[S, F, R], R[S[Point]]] =
    HasParser
      .instance[DrawingAlgebraParserContainer[S, F, R], R[S[Point]]](_.pointParserS, (c, p) => c.copy(pointParserS = p))

  implicit def hasShift[S[_], F[_], R[_]]: HasShift[DrawingAlgebraParserContainer[S, F, R], R] =
    HasShift.instance[DrawingAlgebraParserContainer[S, F, R], R] { (container, algebra) =>
      DrawingAlgebraParserContainer[S, F, R](
        doubleParserS = container.doubleParserS.transformLeaf(p => p.map(algebra.shift)),
        doubleParserF = container.doubleParserF.transformLeaf(p => p.map(algebra.shift)),
        pointParserS = container.pointParserS.transformLeaf(p => p.map(algebra.shift)),
        pointParserF = container.pointParserF.transformLeaf(p => p.map(algebra.shift))
      )
    }
}
