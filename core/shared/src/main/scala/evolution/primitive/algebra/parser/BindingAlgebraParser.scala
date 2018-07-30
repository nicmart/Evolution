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
    container
      .addExtensibleParser(parser[C, T, T])

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

  def buildContainer4[C, T1, T2, T3, T4](container: C)(
    implicit
    hasT1: HasParser[C, F[T1]],
    hasT2: HasParser[C, F[T2]],
    hasT3: HasParser[C, F[T3]],
    hasT4: HasParser[C, F[T4]]
  ): C =
    container
      .addExtensibleParser(parser[C, T1, T1])
      .addExtensibleParser(parser[C, T1, T2])
      .addExtensibleParser(parser[C, T1, T3])
      .addExtensibleParser(parser[C, T1, T4])
      .addExtensibleParser(parser[C, T2, T1])
      .addExtensibleParser(parser[C, T2, T2])
      .addExtensibleParser(parser[C, T2, T3])
      .addExtensibleParser(parser[C, T2, T4])
      .addExtensibleParser(parser[C, T3, T1])
      .addExtensibleParser(parser[C, T3, T2])
      .addExtensibleParser(parser[C, T3, T3])
      .addExtensibleParser(parser[C, T3, T4])
      .addExtensibleParser(parser[C, T4, T1])
      .addExtensibleParser(parser[C, T4, T2])
      .addExtensibleParser(parser[C, T4, T3])
      .addExtensibleParser(parser[C, T4, T4])

  def parser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]]
  ): ExtensibleParser[C, F[Out]] =
    letExtensibleParser[C, Var, Out]
      .extendWith(extensibleLambdaParser[C, Var, Out])
      .extendWith(extensibleFixParser[C, Out])

  private def addVarUsage[T, C](name: String, container: C)(implicit has: HasParser[C, F[T]]): C =
    container.withExtensibleParser[F[T]](container.extensibleParser.transformLeaf { leaf =>
      var0[T](name) | leaf.map(expr => alg.shift(expr))
    })

  private def var0[A](varName: String): Parser[F[A]] =
    varUsage(varName).map(_ => alg.var0)

  private def letExtensibleParser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]]
  ): ExtensibleParser[C, F[Out]] =
    ExtensibleParser(
      Fail,
      self => letParser(self.parser[F[Var]], name => addVarUsage[Var, C](name, self).parser[F[Out]])
    )

  private def letParser[A, B](assignment: Parser[F[A]], body: String => Parser[F[B]]): Parser[F[B]] =
    functionFlatMap[(String, F[A]), F[B]](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => alg.let(name, valueExpr)(bodyExpr))
    })

  private def extensibleLambdaParser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]]
  ): ExtensibleParser[C, F[Out]] =
    ExtensibleParser(Fail, self => lambdaParser(name => addVarUsage[Var, C](name, self).parser[F[Out]]))

  private def lambdaParser[T](body: String => Parser[F[T]]): Parser[F[T]] =
    P(varName ~ "->").flatMap(name => whitespaceWrap(body(name)).map(alg.lambda(name, _)))

  private def fixParser[T](parser: Parser[F[T]]): Parser[F[T]] =
    function1[F[T]]("fix", parser).map(alg.fix)

  private def extensibleFixParser[C, T](implicit hasT: HasParser[C, F[T]]): ExtensibleParser[C, F[T]] =
    ExtensibleParser(Fail, c => fixParser[T](c.parser[F[T]]))
}
