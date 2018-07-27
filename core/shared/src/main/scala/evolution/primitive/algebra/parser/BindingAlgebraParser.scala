package evolution.primitive.algebra.parser

import evolution.data.HasValue
import evolution.geometry.Point
import evolution.primitive.algebra.parser.PrimitiveParsers.whitespaceWrap
import evolution.primitive.algebra.{BindingAlgebra, Type, TypeAlg}
import fastparse.all
import fastparse.noApi.Parser
import ExtensibleParser._
import ParsersContainerOps._

class BindingAlgebraParser[F[_]](alg: BindingAlgebra[F]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def parser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]]
  ): ExtensibleParser[C, F[Out]] =
    ExtensibleParser(
      Fail,
      self => letSyntax(self.parser[Var, F], name => addVarNameFor[Var, C](name, self).parser[Out, F])
    )

  private def addVarNameFor[T, C](name: String, container: C)(implicit has: HasParser[C, F[T]]): C =
    container.withExtensibleParser[T, F](container.extensibleParser[T, F].mapLeaf { leaf =>
      leaf.map(expr => alg.shift(expr)) | var0(name)
    })

  private def var0[A](varName: String): Parser[F[A]] =
    varUsage(varName).map(_ => alg.var0)

  private def letSyntax[A, B](assignment: Parser[F[A]], body: String => Parser[F[B]]): Parser[F[B]] =
    functionFlatMap[(String, F[A]), F[B]](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => alg.let(name, valueExpr)(bodyExpr))
    })
}
