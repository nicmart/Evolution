package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.{
  BindingAlgebra,
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
      new MappedCoreDrawingAlgebra[S, F, λ[α => Parser[R[α]]], ByVarParser[R, ?]](
        new CoreDrawingAlgebraSyntax(alg.drawing),
        to[R],
        from[R]
      )

    override val scalar: ScalarAlgebra[λ[α => ByVarParser[R, S[α]]]] =
      new MappedScalarAlgebra[λ[α => Parser[R[S[α]]]], ByVarParser[λ[α => R[S[α]]], ?]](
        new ScalarAlgebra.Syntax(alg.scalar),
        to[λ[α => R[S[α]]]],
        from[λ[α => R[S[α]]]]
      )

    override val bind: BindingAlgebra[ByVarParser[R, ?], Parser[String]] =
      new BindingAlgebra.Syntax(alg.bind)
  }

  def to[R[_]]: λ[α => Parser[R[α]]] ~> ByVarParser[R, ?] =
    new FunctionK[λ[α => Parser[R[α]]], ByVarParser[R, ?]] {
      override def apply[A](fa: Parser[R[A]]): ByVarParser[R, A] = _ => fa
    }
  def from[R[_]]: ByVarParser[R, ?] ~> λ[α => Parser[R[α]]] =
    new FunctionK[ByVarParser[R, ?], λ[α => Parser[R[α]]]] {
      override def apply[A](fa: ByVarParser[R, A]): Parser[R[A]] = fa(Nil)
    }
}
