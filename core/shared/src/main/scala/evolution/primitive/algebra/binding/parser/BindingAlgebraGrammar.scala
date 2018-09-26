package evolution.primitive.algebra.binding.parser

import cats.{Defer, MonoidK}
import evolution.primitive.algebra.binding.BindingAlgebra

class BindingAlgebraGrammar[R[_], VarName](
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

object BindingAlgebraGrammar {
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
