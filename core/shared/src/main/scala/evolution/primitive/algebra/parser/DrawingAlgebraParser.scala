package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.parser.DrawingAlgebraParser.Container
import fastparse.noApi._

class DrawingAlgebraParser[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]) {
  type RS[A] = R[S[A]]
  type RF[A] = R[F[A]]
  private val coreDrawingAlgebraParser: CoreDrawingAlgebraParser[S, F, R] =
    new CoreDrawingAlgebraParser[S, F, R](alg.drawing)
  private val bindingAlgebraParser: BindingAlgebraParser[R] =
    new BindingAlgebraParser[R](alg.bind)
  private val scalarAlgebraParser: ConstantsAlgebraParser[RS] =
    new ConstantsAlgebraParser[RS](alg.scalar)

  def buildContainer[C](container: C)(
    implicit
    doubleS: HasParser[C, RS, Double],
    doubleF: HasParser[C, RF, Double],
    pointS: HasParser[C, RS, Point],
    pointF: HasParser[C, RF, Point],
    hasParserFunc11: HasParser[C, R, S[Double] => F[Double] => F[Double]],
    hasParserFunc12: HasParser[C, R, S[Double] => F[Double] => F[Point]],
    hasParserFunc21: HasParser[C, R, S[Point] => F[Point] => F[Double]],
    hasParserFunc22: HasParser[C, R, S[Point] => F[Point] => F[Point]],
    hasVariables: HasVariables[C]
  ): C = {
    import HasParser.PushRight._
    val withDrawings = coreDrawingAlgebraParser.buildContainer2[C, Double, Point](container)
    val withScalars = scalarAlgebraParser.buildContainer[C, Double](withDrawings)
    val withLetBindings = bindingAlgebraParser.buildContainer4[C, F[Double], F[Point], S[Double], S[Point]](withScalars)
    withLetBindings
  }

  // TODO this is rubbish
  def container: DrawingAlgebraParser.Container[S, F, R] = {
    import DrawingAlgebraParser.Container._, bindingAlgebraParser._, HasParser.PushRight._
    implicit val p1 = implicitly[HasParser[Container[S, F, R], R, F[Double] => F[Double]]]
    implicit val p2 = implicitly[HasParser[Container[S, F, R], R, F[Point] => F[Double]]]
    implicit val p3 = implicitly[HasParser[Container[S, F, R], R, F[Double] => F[Point]]]
    implicit val p4 = implicitly[HasParser[Container[S, F, R], R, F[Point] => F[Point]]]
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
    implicit def hasDoubleFParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], Lambda[T => R[F[T]]], Double] =
      HasParser
        .instance[Container[S, F, R], Lambda[T => R[F[T]]], Double](
          _.dependentDoubleParserF,
          (c, p) => c.copy(dependentDoubleParserF = p)
        )

    implicit def hasDoubleSParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], Lambda[T => R[S[T]]], Double] =
      HasParser
        .instance[Container[S, F, R], Lambda[T => R[S[T]]], Double](
          _.dependentDoubleParserS,
          (c, p) => c.copy(dependentDoubleParserS = p)
        )

    implicit def hasPointFParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], Lambda[T => R[F[T]]], Point] =
      HasParser
        .instance[Container[S, F, R], Lambda[T => R[F[T]]], Point](
          _.dependentPointParserF,
          (c, p) => c.copy(dependentPointParserF = p)
        )

    implicit def hasPointSParser[S[_], F[_], R[_]]: HasParser[Container[S, F, R], Lambda[T => R[S[T]]], Point] =
      HasParser
        .instance[Container[S, F, R], Lambda[T => R[S[T]]], Point](
          _.dependentPointParserS,
          (c, p) => c.copy(dependentPointParserS = p)
        )

    implicit def hasVariables[S[_], F[_], R[_]]: HasVariables[Container[S, F, R]] =
      HasVariables.instance[Container[S, F, R]](_.variables, (name, c) => c.addVariable(name))

  }
}
