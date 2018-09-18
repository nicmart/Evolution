package evolution.primitive.algebra.parser

import evolution.primitive.algebra.BindingAlgebra
import ParserConfig.White._
import fastparse.noApi._
import PrimitiveParsers._
import cats.{Defer, MonoidK}
import fastparse.noApi

object BindingAlgebra {
  class Syntax[R[_]](alg: BindingAlgebra[R, String])
      extends BindingAlgebra[λ[α => List[String] => Parser[R[α]]], Parser[String]] {
    type ByVarParser[A] = List[String] => Parser[R[A]]

    override def varName(name: String): Parser[String] =
      P(name).!

    override def var0[A]: ByVarParser[A] =
      vars =>
        vars.headOption.fold[Parser[R[A]]](Fail) { currentVar =>
          varUsage(currentVar).map(_ => alg.var0)
      }

    override def shift[A](expr: ByVarParser[A]): ByVarParser[A] = {
      case _ :: tail => expr(tail).map(alg.shift)
      case _ => Fail
    }

    override def let[A, B](variableName: Parser[String], value: ByVarParser[A])(expr: ByVarParser[B]): ByVarParser[B] =
      vars =>
        functionFlatMap[(String, R[A]), R[B]](function2("let", variableName, value(vars)), {
          case (parsedVariableName, parsedValue) =>
            expr(parsedVariableName :: vars).map { parsedExpression =>
              alg.let(parsedVariableName, parsedValue)(parsedExpression)
            }
        })

    override def lambda[A, B](variableName: Parser[String], expr: ByVarParser[B]): ByVarParser[A => B] =
      vars =>
        functionFlatMap[String, R[A => B]](
          function1("lambda", variableName),
          parsedVariableName =>
            expr(parsedVariableName :: vars).map { parsedExpression =>
              alg.lambda(parsedVariableName, parsedExpression)
          }
      )

    override def app[A, B](f: ByVarParser[A => B], a: ByVarParser[A]): ByVarParser[B] =
      vars =>
        function2("app", f(vars), a(vars)).map {
          case (parsedFunction, parsedArgument) => alg.app(parsedFunction, parsedArgument)
      }

    override def fix[A](expr: ByVarParser[A]): ByVarParser[A] =
      vars => function1("fix", expr("self" :: vars)).map(alg.fix)
  }

  class Grammar[R[_], Type[_], VarName](
    self: Expressions[R, Type],
    syntax: BindingAlgebra[R, VarName],
    varNameSyntax: VarName,
    orMonoid: MonoidK[R],
    types: List[Type[_]]
  ) extends Expressions[R, Type] {

    override def value[T](t: Type[T]): R[T] =
      valueRec(self.value(t))

    override def func[T1, T2](t1: Type[T1], r2: R[T2]): R[T1 => T2] =
      or(syntax.lambda(varNameSyntax, r2), valueRec(self.func(t1, r2)))

    private def valueRec[T](t: R[T]): R[T] =
      or(syntax.var0, syntax.shift(t), syntax.fix(t), allLetExpressions(t), allAppExpressions(t))

    private def letExpression[T1, T2](t1: Type[T1], r2: R[T2]): R[T2] =
      syntax.let(varNameSyntax, self.value(t1))(r2)

    private def allLetExpressions[T](t: R[T]): R[T] =
      or(types.map(s => letExpression(s, t)): _*)

    private def appExpression[T1, T2](t1: Type[T1], r2: R[T2]): R[T2] =
      syntax.app(self.func(t1, r2), self.value(t1))

    private def allAppExpressions[T](t: R[T]): R[T] =
      or(types.map(s => appExpression(s, t)): _*)

    private def or[T](expressions: R[T]*): R[T] =
      expressions.foldLeft(orMonoid.empty[T])(orMonoid.combineK[T])
  }

  trait Expressions[R[_], Type[_]] {
    def value[T](t: Type[T]): R[T]
    def func[T1, T2](t1: Type[T1], r2: R[T2]): R[T1 => T2]
  }

  class OrExpressions[R[_], Type[_]](
    orMonoid: MonoidK[R],
    defer: Defer[R],
    multipleExpressions: List[Expressions[R, Type]]
  ) extends Expressions[R, Type] {
    override def value[T](t: Type[T]): R[T] = combine(_.value(t))
    override def func[T1, T2](t1: Type[T1], r2: R[T2]): R[T1 => T2] = combine(_.func(t1, r2))

    private def combine[T](f: Expressions[R, Type] => R[T]): R[T] =
      multipleExpressions
        .map(expression => defer.defer(f(expression)))
        .foldLeft(orMonoid.empty[T])(orMonoid.combineK)
  }

  class LazyExpressions[R[_], Type[_]](expressions: => Expressions[R, Type]) extends Expressions[R, Type] {
    override def value[T](t: Type[T]): R[T] = expressions.value(t)
    override def func[T1, T2](t1: Type[T1], r2: R[T2]): R[T1 => T2] = expressions.func(t1, r2)
  }

  class EmptyExpressions[R[_], Type[_]](monoid: MonoidK[R]) extends Expressions[R, Type] {
    override def value[T](t: Type[T]): R[T] = monoid.empty[T]
    override def func[T1, T2](t1: Type[T1], r2: R[T2]): R[T1 => T2] = monoid.empty[T1 => T2]
  }

  def fixExpressions[R[_], Type[_]](
    dependentExpressions: Expressions[R, Type] => Expressions[R, Type]
  ): Expressions[R, Type] =
    dependentExpressions(new LazyExpressions(fixExpressions(dependentExpressions)))

  def fixMultipleExpressions[R[_], Type[_]](
    orMonoid: MonoidK[R],
    defer: Defer[R],
    multipleDependentExpressions: List[Expressions[R, Type] => Expressions[R, Type]]
  ): Expressions[R, Type] = {

    def dependentExpressions(expressions: Expressions[R, Type]): Expressions[R, Type] =
      new OrExpressions(
        orMonoid,
        defer,
        multipleDependentExpressions.map(dependentExpressions => dependentExpressions(expressions))
      )

    fixExpressions(dependentExpressions)
  }
}
