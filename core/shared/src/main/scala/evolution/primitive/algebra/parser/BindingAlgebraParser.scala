package evolution.primitive.algebra.parser

import evolution.primitive.algebra.BindingAlgebra
import ParsersContainerOps._

class BindingAlgebraParser[F[_]](alg: BindingAlgebra[F]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer1[C, T](container: C)(
    implicit
    hasVar: HasParser[C, F, T],
    hasVars: HasVariables[C]
  ): C =
    container
      .addParser[F, T](parser[C, T, T])

  def buildContainer2[C, T1, T2](container: C)(
    implicit
    hasT1: HasParser[C, F, T1],
    hasT2: HasParser[C, F, T2],
    hasVars: HasVariables[C]
  ): C =
    container
      .addParser[F, T1](parser[C, T1, T1])
      .addParser[F, T2](parser[C, T1, T2])
      .addParser[F, T1](parser[C, T2, T1])
      .addParser[F, T2](parser[C, T2, T2])

  def buildContainer4[C, T1, T2, T3, T4](container: C)(
    implicit
    hasT1: HasParser[C, F, T1],
    hasT2: HasParser[C, F, T2],
    hasT3: HasParser[C, F, T3],
    hasT4: HasParser[C, F, T4],
    hasVars: HasVariables[C]
  ): C =
    container
      .addParser[F, T1](parser[C, T1, T1])
      .addParser[F, T2](parser[C, T1, T2])
      .addParser[F, T3](parser[C, T1, T3])
      .addParser[F, T4](parser[C, T1, T4])
      .addParser[F, T1](parser[C, T2, T1])
      .addParser[F, T2](parser[C, T2, T2])
      .addParser[F, T3](parser[C, T2, T3])
      .addParser[F, T4](parser[C, T2, T4])
      .addParser[F, T1](parser[C, T3, T1])
      .addParser[F, T2](parser[C, T3, T2])
      .addParser[F, T3](parser[C, T3, T3])
      .addParser[F, T4](parser[C, T3, T4])
      .addParser[F, T1](parser[C, T4, T1])
      .addParser[F, T2](parser[C, T4, T2])
      .addParser[F, T3](parser[C, T4, T3])
      .addParser[F, T4](parser[C, T4, T4])

  def parser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F, Var],
    hasOut: HasParser[C, F, Out],
    hasVars: HasVariables[C]
  ): DependentParser[C, F[Out]] =
    dependentLetParser[C, Var, Out]
      .or(dependentVariableParser)
      //.or(dependentLambdaParser[C, Var, Out])
      .or(dependentAppParser[C, Var, Out])
      .or(dependentFixParser[C, Out])

  private def var0[A](varName: String): Parser[F[A]] =
    varUsage(varName).map(_ => alg.var0)

  private def dependentLetParser[C, Var, Out](
    implicit
    hasVar: HasParser[C, F, Var],
    hasOut: HasParser[C, F, Out],
    hasVars: HasVariables[C]
  ): DependentParser[C, F[Out]] =
    DependentParser(container => letParser(container.parser[F, Var], name => container.addVar(name).parser[F, Out]))

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
    hasOut: HasParser[C, F, Out],
    hasVariables: HasVariables[C]
  ): DependentParser[C, F[Var => Out]] =
    DependentParser(container => lambdaParser[Var, Out](name => container.addVar(name).parser[F, Out]))

  private def lambdaParser[A, B](body: String => Parser[F[B]]): Parser[F[A => B]] =
    P(varName ~ "->").flatMap(name => whitespaceWrap(body(name)).map(alg.lambda(name, _)))

  private def fixParser[T](parser: Parser[F[T]]): Parser[F[T]] =
    function1[F[T]]("fix", P("self" ~ "->" ~ parser)).map(alg.fix)

  private def dependentFixParser[C, T](
    implicit hasT: HasParser[C, F, T],
    hasVariables: HasVariables[C]
  ): DependentParser[C, F[T]] =
    DependentParser(c => fixParser[T](c.addVar("self").parser[F, T]))

  private def appParser[A, B](functionParser: Parser[F[A => B]], argParser: Parser[F[A]]): Parser[F[B]] =
    function2("app", functionParser, argParser).map { case (f, arg) => alg.app[A, B](f, arg) }

  private def dependentAppParser[C, A, B](
    implicit
    hasA: HasParser[C, F, A],
    hasB: HasParser[C, F, B],
    hasVariables: HasVariables[C]
  ): DependentParser[C, F[B]] = {
    val lambda = dependentLambdaParser[C, A, B]
    DependentParser(c => appParser[A, B](lambda.parser(c), c.parser[F, A]))
  }

  implicit def hasFunctions[C, Var, Out](
    implicit hasVariables: HasVariables[C],
    hasOut: HasParser[C, F, Out]
  ): HasParser[C, F, Var => Out] =
    new HasParser[C, F, Var => Out] {
      override def get(x: C): DependentParser[C, F[Var => Out]] = dependentLambdaParser[C, Var, Out]
      override def set(x: C, v: DependentParser[C, F[Var => Out]]): C = ???
    }
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
