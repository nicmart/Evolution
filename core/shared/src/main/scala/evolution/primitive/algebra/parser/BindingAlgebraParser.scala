package evolution.primitive.algebra.parser

import evolution.primitive.algebra.BindingAlgebra
import ParsersContainerOps._
import evolution.primitive.algebra.parser.DependentParser.HasParser

class BindingAlgebraParser[F[_]](alg: BindingAlgebra[F]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer1[C, T](container: C)(
    implicit
    hasVar: HasParser[C, F[T]],
    hasVars: HasVariables[C]
  ): C =
    container
      .addParser(parser[C, T, T])

  def buildContainer2[C, T1, T2](container: C)(
    implicit
    hasT1: HasParser[C, F[T1]],
    hasT2: HasParser[C, F[T2]],
    hasVars: HasVariables[C]
  ): C =
    container
      .addParser(parser[C, T1, T1])
      .addParser(parser[C, T1, T2])
      .addParser(parser[C, T2, T1])
      .addParser(parser[C, T2, T2])

  def buildContainer4[C, T1, T2, T3, T4](container: C)(
    implicit
    hasT1: HasParser[C, F[T1]],
    hasT2: HasParser[C, F[T2]],
    hasT3: HasParser[C, F[T3]],
    hasT4: HasParser[C, F[T4]],
    hasVars: HasVariables[C]
  ): C =
    container
      .addParser(parser[C, T1, T1])
      .addParser(parser[C, T1, T2])
      .addParser(parser[C, T1, T3])
      .addParser(parser[C, T1, T4])
      .addParser(parser[C, T2, T1])
      .addParser(parser[C, T2, T2])
      .addParser(parser[C, T2, T3])
      .addParser(parser[C, T2, T4])
      .addParser(parser[C, T3, T1])
      .addParser(parser[C, T3, T2])
      .addParser(parser[C, T3, T3])
      .addParser(parser[C, T3, T4])
      .addParser(parser[C, T4, T1])
      .addParser(parser[C, T4, T2])
      .addParser(parser[C, T4, T3])
      .addParser(parser[C, T4, T4])

  def parser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]],
    hasVars: HasVariables[C]
  ): DependentParser[C, F[Out]] =
    dependentLetParser[C, Var, Out]
      .or(dependentVariableParser)
      .or(dependentLambdaParser[C, Var, Out])
      .or(dependentAppParser[C, Out, Var])
      .or(dependentFixParser[C, Out])

  private def var0[A](varName: String): Parser[F[A]] =
    varUsage(varName).map(_ => alg.var0)

  private def dependentLetParser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]],
    hasVars: HasVariables[C]
  ): DependentParser[C, F[Out]] =
    DependentParser(container => letParser(container.parser[F[Var]], name => container.addVar(name).parser[F[Out]]))

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
    implicit
    hasVar: HasParser[C, F[Var]],
    hasOut: HasParser[C, F[Out]],
    hasVariables: HasVariables[C]
  ): DependentParser[C, F[Out]] =
    DependentParser(container => lambdaParser(name => container.addVar(name).parser[F[Out]]))

  private def lambdaParser[T](body: String => Parser[F[T]]): Parser[F[T]] = ???
  //P(varName ~ "->").flatMap(name => whitespaceWrap(body(name)).map(alg.lambda(name, _)))

  private def fixParser[T](parser: Parser[F[T]]): Parser[F[T]] = ???
  //function1[F[T]]("fix", parser).map(alg.fix)

  private def dependentFixParser[C, T](implicit hasT: HasParser[C, F[T]]): DependentParser[C, F[T]] =
    DependentParser(c => fixParser[T](c.parser[F[T]]))

  private def appParser[A, B](functionParser: Parser[F[A]], argParser: Parser[F[B]]): Parser[F[A]] = ???
//    function2("app", functionParser, argParser).map { case (f, arg) => alg.app[A, B](f, arg) }

  private def dependentAppParser[C, A, B](
    implicit hasA: HasParser[C, F[A]],
    hasB: HasParser[C, F[B]]
  ): DependentParser[C, F[A]] =
    DependentParser(c => appParser(c.parser[F[A]], c.parser[F[B]]))
}

trait HasVariables[C] {
  def vars(c: C): List[String]
  def addVar(name: String, c: C): C
}

object HasVariables {
  def instance[C](_vars: C => List[String], _addVar: (String, C) => C): HasVariables[C] = new HasVariables[C] {
    override def vars(c: C): List[String] = _vars(c)
    override def addVar(name: String, c: C): C = _addVar(name, c)
  }
}
