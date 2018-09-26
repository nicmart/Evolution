package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.{
  BindingAlgebra,
  ContextualCoreDrawingAlgebra,
  ContextualScalarAlgebra,
  CoreDrawingAlgebra,
  DrawingAlgebra,
  MappedCoreDrawingAlgebra,
  MappedScalarAlgebra,
  ScalarAlgebra,
  parser
}
import evolution.primitive.algebra.parser.DrawingAlgebraParser.Container
import ParserConfig.White._
import fastparse.noApi._
import PrimitiveParsers._
import cats.arrow.FunctionK
import cats.~>
import evolution.primitive.algebra.parser.BindingAlgebra.ByVarParser
import fastparse.noApi

object DrawingAlgebra {

  class Syntax[S[_], F[_], R[_]](alg: DrawingAlgebra[S, F, R, String])
      extends DrawingAlgebra[S, F, ByVarParser[R, ?], Parser[String]] {

    override val drawing: CoreDrawingAlgebra[S, F, ByVarParser[R, ?]] =
      new ContextualCoreDrawingAlgebra[S, F, λ[α => Parser[R[α]]], List[String]](
        new CoreDrawingAlgebraSyntax(alg.drawing)
      )

    override val scalar: ScalarAlgebra[λ[α => ByVarParser[R, S[α]]]] =
      new ContextualScalarAlgebra[λ[α => Parser[R[S[α]]]], List[String]](new ScalarAlgebra.Syntax(alg.scalar))

    override val bind: BindingAlgebra[ByVarParser[R, ?], Parser[String]] =
      new BindingAlgebra.Syntax(alg.bind)
  }
}
