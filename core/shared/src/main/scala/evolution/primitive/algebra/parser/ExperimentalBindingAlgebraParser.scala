package evolution.primitive.algebra.parser

import evolution.primitive.algebra.BindingAlgebra
import ParsersContainerOps._

import scala.collection.immutable

class ExperimentalBindingAlgebraParser[F[_]](alg: BindingAlgebra[F]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  private def var0[A](varName: String): Parser[F[A]] =
    varUsage(varName).map(_ => alg.var0)

  private def dependentLetParser[C, In, Out](
    in: DependentParser[C, F[In]],
    out: DependentParser[C, F[Out]],
    hasVars: HasVariables[C]
  ): DependentParser[C, F[Out]] = {
    // TODO
    implicit val hasVarsImpl: HasVariables[C] = hasVars
    DependentParser(container => letParser(in.parser(container), name => out.parser(container.addVar(name))))
  }

  private def variableParser[Var](vars: List[String]): Parser[F[Var]] = vars match {
    case Nil => Fail
    case head :: tail => var0[Var](head) | variableParser[Var](tail).map(alg.shift)
  }

  private def dependentVariableParser[C: HasVariables, Var]: DependentParser[C, F[Var]] =
    DependentParser(c => variableParser(c.vars))

  private def letParser[A, B](assignment: Parser[F[A]], body: String => Parser[F[B]]): Parser[F[B]] =
    functionFlatMap[(String, F[A]), F[B]](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => alg.let(name, valueExpr)(bodyExpr))
    })

  private def dependentLambdaParser[C, Var, Out](
    out: DependentParser[C, F[Out]],
    hasVariables: HasVariables[C]
  ): DependentParser[C, F[Var => Out]] = {
    // TODO
    implicit val implVars: HasVariables[C] = hasVariables
    DependentParser(container => lambdaParser[Var, Out](name => out.parser(container.addVar(name))))
  }

  private def lambdaParser[A, B](body: String => Parser[F[B]]): Parser[F[A => B]] =
    P(varName ~ "->").flatMap(name => whitespaceWrap(body(name)).map(alg.lambda(name, _)))

  private def fixParser[T](parser: Parser[F[T]]): Parser[F[T]] =
    function1[F[T]]("fix", P("self" ~ "->" ~ parser)).map(alg.fix)

  private def dependentFixParser[C, T](
    tParser: DependentParser[C, F[T]],
    hasVariables: HasVariables[C]
  ): DependentParser[C, F[T]] = {
    // TODO
    implicit val implVariables: HasVariables[C] = hasVariables
    DependentParser(c => fixParser[T](tParser.parser(c.addVar("self"))))
  }

  private def appParser[A, B](functionParser: Parser[F[A => B]], argParser: Parser[F[A]]): Parser[F[B]] =
    function2("app", functionParser, argParser).map { case (f, arg) => alg.app[A, B](f, arg) }

  private def dependentAppParser[C, A, B](
    aParser: DependentParser[C, F[A]],
    lambdaParser: DependentParser[C, F[A => B]],
    hasVariables: HasVariables[C]
  ): DependentParser[C, F[B]] = {
    // TODO
    implicit val implVariables: HasVariables[C] = hasVariables
    DependentParser(c => appParser[A, B](lambdaParser.parser(c), aParser.parser(c)))
  }

  trait ParserSet {
    type T
    def hasVariables: HasVariables[ParserSet]
    def all: List[ParserSet]
    def parser: DependentParser[ParserSet, F[T]]
    def lambdaParser(other: ParserSet): DependentParser[ParserSet, F[other.T => T]] =
      dependentLambdaParser(parser, hasVariables)
        .or(next(other).combinedAppParsers)
    def next[S](other: ParserSet): ParserSet.Aux[other.T => T]

    private def combinedAppParsers: DependentParser[ParserSet, F[T]] = {
      val allAppParsers: List[DependentParser[ParserSet, F[T]]] =
        all.map { otherParserSet =>
          val lambda: DependentParser[ParserSet, F[otherParserSet.T => T]] = lambdaParser(otherParserSet)
          dependentAppParser[ParserSet, otherParserSet.T, T](otherParserSet.parser, lambda, hasVariables)
        }
      allAppParsers.reduceLeft(_.or(_))
    }
  }

  object ParserSet {
    type Aux[X] = ParserSet { type T = X }
  }
}
