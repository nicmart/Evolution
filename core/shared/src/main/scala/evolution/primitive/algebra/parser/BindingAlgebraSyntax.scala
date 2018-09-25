package evolution.primitive.algebra.parser

import evolution.primitive.algebra.BindingAlgebra
import ParserConfig.White._
import fastparse.noApi._
import PrimitiveParsers._
import cats.{Defer, MonoidK}

object BindingAlgebra {
  type ByVarParser[R[_], A] = List[String] => Parser[R[A]]
  class Syntax[R[_]](alg: BindingAlgebra[R, String]) extends BindingAlgebra[ByVarParser[R, ?], Parser[String]] {

    override def varName(name: String): Parser[String] =
      P(name).!

    override def var0[A]: ByVarParser[R, A] =
      vars =>
        vars.headOption.fold[Parser[R[A]]](Fail) { currentVar =>
          varUsage(currentVar).map(_ => alg.var0)
      }

    override def shift[A](expr: ByVarParser[R, A]): ByVarParser[R, A] = {
      case _ :: tail => expr(tail).map(alg.shift)
      case _ => Fail
    }

    override def let[A, B](variableName: Parser[String], value: ByVarParser[R, A])(
      expr: ByVarParser[R, B]
    ): ByVarParser[R, B] =
      vars =>
        functionFlatMap[(String, R[A]), R[B]](function2("let", variableName, value(vars)), {
          case (parsedVariableName, parsedValue) =>
            expr(parsedVariableName :: vars).map { parsedExpression =>
              alg.let(parsedVariableName, parsedValue)(parsedExpression)
            }
        })

    override def lambda[A, B](variableName: Parser[String], expr: ByVarParser[R, B]): ByVarParser[R, A => B] =
      vars =>
        functionFlatMap[String, R[A => B]](
          function1("lambda", variableName),
          parsedVariableName =>
            expr(parsedVariableName :: vars).map { parsedExpression =>
              alg.lambda(parsedVariableName, parsedExpression)
          }
      )

    override def app[A, B](f: ByVarParser[R, A => B], a: ByVarParser[R, A]): ByVarParser[R, B] =
      vars =>
        function2("app", f(vars), a(vars)).map {
          case (parsedFunction, parsedArgument) => alg.app(parsedFunction, parsedArgument)
      }

    override def fix[A](expr: ByVarParser[R, A => A]): ByVarParser[R, A] =
      vars => function1("fix", expr("self" :: vars)).map(alg.fix)
  }

  class Grammar[R[_], VarName](
    self: Expressions[R],
    syntax: BindingAlgebra[R, VarName],
    varNameSyntax: VarName,
    orMonoid: MonoidK[R],
    all: List[R[_]]
  ) extends Expressions[R] {

    override def value[T](t: R[T]): R[T] =
      or(t, valueRec(self.value(t)))

    override def func[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
      or(syntax.lambda(varNameSyntax, t2), valueRec(self.func(t1, t2)))

    private def valueRec[T](t: R[T]): R[T] =
      or(syntax.var0, syntax.shift(t), syntax.fix(self.func(t, t)), allLetExpressions(t), allAppExpressions(t))

    private def letExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
      syntax.let(varNameSyntax, self.value(t1))(t2)

    private def allLetExpressions[T](t: R[T]): R[T] =
      or(all.map(s => letExpression(s, t)): _*)

    private def appExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
      syntax.app(self.func(t1, t2), t1)

    private def allAppExpressions[T](t: R[T]): R[T] =
      or(all.map(s => appExpression(self.value(s), t)): _*)

    private def or[T](expressions: R[T]*): R[T] =
      expressions.foldLeft(orMonoid.empty[T])(orMonoid.combineK[T])
  }

  trait Expressions[R[_]] {
    def value[T](t: R[T]): R[T]
    def func[T1, T2](t1: R[T1], r2: R[T2]): R[T1 => T2]
  }

  class OrExpressions[R[_]](orMonoid: MonoidK[R], defer: Defer[R], multipleExpressions: List[Expressions[R]])
      extends Expressions[R] {
    override def value[T](t: R[T]): R[T] = combine(_.value(t))
    override def func[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] = combine(_.func(t1, t2))

    private def combine[T](f: Expressions[R] => R[T]): R[T] =
      multipleExpressions
        .map(expression => defer.defer(f(expression)))
        .foldLeft(orMonoid.empty[T])(orMonoid.combineK)
  }

  class LazyExpressions[R[_]](expressions: => Expressions[R]) extends Expressions[R] {
    override def value[T](t: R[T]): R[T] = expressions.value(t)
    override def func[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] = expressions.func(t1, t2)
  }

  def fixExpressions[R[_]](dependentExpressions: Expressions[R] => Expressions[R]): Expressions[R] =
    dependentExpressions(new LazyExpressions(fixExpressions(dependentExpressions)))

  def fixMultipleExpressions[R[_]](
    orMonoid: MonoidK[R],
    defer: Defer[R],
    multipleDependentExpressions: List[Expressions[R] => Expressions[R]]
  ): Expressions[R] = {

    def dependentExpressions(expressions: Expressions[R]): Expressions[R] =
      new OrExpressions(
        orMonoid,
        defer,
        multipleDependentExpressions.map(dependentExpressions => dependentExpressions(expressions))
      )

    fixExpressions(dependentExpressions)
  }
}
