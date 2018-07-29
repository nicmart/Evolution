package evolution.primitive.algebra.parser

import evolution.primitive.algebra.BindingAlgebra
import fastparse.all
import fastparse.noApi.Parser
import ExtensibleParser._
import ParsersContainerOps._

class BindingAlgebraParser[F[_]](alg: BindingAlgebra[F]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer1[C, T](container: C)(
    implicit
    hasVar: HasParser[C, F[T]]
  ): C =
    container.addExtensibleParser(parser[C, T, T])

  def buildContainer2[C, T1, T2](container: C)(
    implicit
    hasT1: HasParser[C, F[T1]],
    hasT2: HasParser[C, F[T2]]
  ): C =
    container
      .addExtensibleParser(parser[C, T1, T1])
      .addExtensibleParser(parser[C, T1, T2])
      .addExtensibleParser(parser[C, T2, T1])
      .addExtensibleParser(parser[C, T2, T2])

  def parser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]]
  ): ExtensibleParser[C, F[Out]] =
    ExtensibleParser(
      Fail,
      self => letParser(self.parser[F[Var]], name => addVarUsage[Var, C](name, self).parser[F[Out]])
    )

  private def addVarUsage[T, C](name: String, container: C)(implicit has: HasParser[C, F[T]]): C =
    container.withExtensibleParser[F[T]](container.extensibleParser.transformLeaf { leaf =>
      leaf.map(expr => alg.shift(expr)) | var0(name)
    })

  private def var0[A](varName: String): Parser[F[A]] =
    varUsage(varName).map(_ => alg.var0)

  private def letParser[A, B](assignment: Parser[F[A]], body: String => Parser[F[B]]): Parser[F[B]] =
    functionFlatMap[(String, F[A]), F[B]](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => alg.let(name, valueExpr)(bodyExpr))
    })
}
