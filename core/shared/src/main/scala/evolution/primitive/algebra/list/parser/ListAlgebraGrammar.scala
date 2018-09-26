package evolution.primitive.algebra.list.parser
import cats.{Defer, MonoidK}
import evolution.primitive.algebra.list.ListAlgebra

class ListAlgebraGrammar[S[_], F[_], R[_]](
  self: Expressions[S, F, R],
  syntax: ListAlgebra[S, F, R],
  orMonoid: MonoidK[R],
  types: List[Type[S, F, R, _]]
) extends Expressions[S, F, R] {

  override def static[T](t: Type[S, F, R, T]): R[S[T]] = failure

  override def evolution[T](t: Type[S, F, R, T]): R[F[T]] =
    or(
      syntax.empty[T],
      syntax.cons(self.static(t), self.evolution(t)),
      syntax.mapEmpty(self.evolution(t))(self.evolution(t)),
      or(allMapConsExpressions(t): _*)
    )

  override def mapConsFunction[T1, T2](t1: Type[S, F, R, T1], t2: Type[S, F, R, T2]): R[S[T1] => F[T1] => F[T2]] =
    failure

  private def or[T](expressions: R[T]*): R[T] =
    expressions.foldLeft(orMonoid.empty[T])(orMonoid.combineK[T])

  private def mapConsExpression[T1, T2](t1: Type[S, F, R, T1], t2: Type[S, F, R, T2]): R[F[T2]] =
    syntax.mapCons(self.evolution(t1))(self.mapConsFunction(t1, t2))

  private def allMapConsExpressions[T](t: Type[S, F, R, T]): Seq[R[F[T]]] =
    types.map(s => mapConsExpression(s, t))

  private def failure[T]: R[T] = orMonoid.empty[T]
}

trait Expressions[S[_], F[_], R[_]] {
  def static[T](t: Type[S, F, R, T]): R[S[T]]
  def evolution[T](t: Type[S, F, R, T]): R[F[T]]
  def mapConsFunction[T1, T2](t1: Type[S, F, R, T1], t2: Type[S, F, R, T2]): R[S[T1] => F[T1] => F[T2]]
}
object Expressions {
  def fix[S[_], F[_], R[_]](dependentExpressions: Expressions[S, F, R] => Expressions[S, F, R]): Expressions[S, F, R] =
    dependentExpressions(new LazyExpressions(fix[S, F, R](dependentExpressions)))

  def fixMultipleExpressions[S[_], F[_], R[_]](
    orMonoid: MonoidK[R],
    defer: Defer[R],
    multipleDependentExpressions: List[Expressions[S, F, R] => Expressions[S, F, R]]
  ): Expressions[S, F, R] = {

    def dependentExpressions(expressions: Expressions[S, F, R]): Expressions[S, F, R] =
      OrExpressions[S, F, R](
        orMonoid,
        defer,
        multipleDependentExpressions.map(dependentExpression => dependentExpression(expressions))
      )

    fix[S, F, R](dependentExpressions)
  }
}

case class Type[S[_], F[_], R[_], T](static: R[S[T]], evolution: R[F[T]])

case class OrExpressions[S[_], F[_], R[_]](
  orMonoid: MonoidK[R],
  defer: Defer[R],
  multipleExpressions: List[Expressions[S, F, R]]
) extends Expressions[S, F, R] {
  override def static[T](t: Type[S, F, R, T]): R[S[T]] =
    combine[S[T]](_.static[T](t))
  override def evolution[T](t: Type[S, F, R, T]): R[F[T]] =
    combine[F[T]](_.evolution[T](t))
  override def mapConsFunction[T1, T2](t1: Type[S, F, R, T1], t2: Type[S, F, R, T2]): R[S[T1] => F[T1] => F[T2]] =
    combine[S[T1] => F[T1] => F[T2]](_.mapConsFunction(t1, t2))

  private def combine[T](f: Expressions[S, F, R] => R[T]): R[T] =
    multipleExpressions
      .map(expression => defer.defer(f(expression)))
      .foldLeft(orMonoid.empty[T])(orMonoid.combineK)
}

class LazyExpressions[S[_], F[_], R[_]](expressions: => Expressions[S, F, R]) extends Expressions[S, F, R] {
  override def static[T](t: Type[S, F, R, T]): R[S[T]] =
    expressions.static(t)
  override def evolution[T](t: Type[S, F, R, T]): R[F[T]] =
    expressions.evolution(t)
  override def mapConsFunction[T1, T2](t1: Type[S, F, R, T1], t2: Type[S, F, R, T2]): R[S[T1] => F[T1] => F[T2]] =
    expressions.mapConsFunction(t1, t2)
}
