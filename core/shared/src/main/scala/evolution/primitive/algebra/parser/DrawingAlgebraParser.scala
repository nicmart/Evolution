package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.DrawingAlgebra
import evolution.primitive.algebra.parser.DependentParser.HasParser
import fastparse.noApi._

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
    hasVariables: HasVariables[C]
  ): C = {
    val withDrawings = coreDrawingAlgebraParser.buildContainer2[C, Double, Point](container)
    val withScalars = scalarAlgebraParser.buildContainer[C, Double](withDrawings)
    val withLetBindings = bindingAlgebraParser.buildContainer4[C, F[Double], F[Point], S[Double], S[Point]](withScalars)
    withLetBindings
  }

  def container: DrawingAlgebraParser.Container[S, F, R] = {
    import DrawingAlgebraParser.Container._
    buildContainer(DrawingAlgebraParser.Container.empty[S, F, R])
  }
}

object DrawingAlgebraParser {

  case class Container[S[_], F[_], R[_]](
    dependentDoubleParserS: DependentParser[Container[S, F, R], R[S[Double]]],
    dependentDoubleParserF: DependentParser[Container[S, F, R], R[F[Double]]],
    dependentPointParserS: DependentParser[Container[S, F, R], R[S[Point]]],
    dependentPointParserF: DependentParser[Container[S, F, R], R[F[Point]]],
    variables: List[String]
  ) {
    def addVariable(name: String): Container[S, F, R] =
      copy(variables = name :: variables)

    def doubleParserS: Parser[R[S[Double]]] = dependentDoubleParserS.parser(this)
    def doubleParserF: Parser[R[F[Double]]] = dependentDoubleParserF.parser(this)
    def pointParserS: Parser[R[S[Point]]] = dependentPointParserS.parser(this)
    def pointParserF: Parser[R[F[Point]]] = dependentPointParserF.parser(this)
  }

  object Container {
    import ParserConfig.White._
    import fastparse.noApi._

    implicit def ops[S[_], F[_], R[_]](container: Container[S, F, R]): ParsersContainerOps[Container[S, F, R]] =
      new ParsersContainerOps[Container[S, F, R]](container)

    def empty[S[_], F[_], R[_]]: Container[S, F, R] =
      Container[S, F, R](
        DependentParser.empty,
        DependentParser.empty,
        DependentParser.empty,
        DependentParser.empty,
        List.empty
      )
    implicit def hasDoubleFParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], R[F[Double]]] =
      HasParser
        .instance[Container[S, F, R], R[F[Double]]](
          _.dependentDoubleParserF,
          (c, p) => c.copy(dependentDoubleParserF = p)
        )

    implicit def hasDoubleSParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], R[S[Double]]] =
      HasParser
        .instance[Container[S, F, R], R[S[Double]]](
          _.dependentDoubleParserS,
          (c, p) => c.copy(dependentDoubleParserS = p)
        )

    implicit def hasPointFParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], R[F[Point]]] =
      HasParser
        .instance[Container[S, F, R], R[F[Point]]](_.dependentPointParserF, (c, p) => c.copy(dependentPointParserF = p))

    implicit def hasPointSParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], R[S[Point]]] =
      HasParser
        .instance[Container[S, F, R], R[S[Point]]](_.dependentPointParserS, (c, p) => c.copy(dependentPointParserS = p))

    implicit def hasVariables[S[_], F[_], R[_]]: HasVariables[Container[S, F, R]] =
      HasVariables.instance[Container[S, F, R]](_.variables, (name, c) => c.addVariable(name))
  }
}
