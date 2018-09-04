package evolution.primitive.algebra.parser

import cats.{Eval, MonoidK}
import evolution.primitive.algebra.CoreDrawingAlgebra
import ParserConfig.White._
import PrimitiveParsers._
import fastparse.noApi._

class CoreDrawingSyntax[S[_], F[_], R[_]](alg: CoreDrawingAlgebra[S, F, R])
    extends CoreDrawingAlgebra[S, F, Lambda[T => Parser[R[T]]]] {

  override def empty[A]: Parser[R[F[A]]] =
    P("empty").map(_ => alg.empty)

  override def cons[A](head: Parser[R[S[A]]], tail: Parser[R[F[A]]]): Parser[R[F[A]]] =
    function2("cons", head, tail).map[R[F[A]]] { case (h, t) => alg.cons(h, t) }

  override def mapEmpty[A](eva: Parser[R[F[A]]])(eva2: Parser[R[F[A]]]): Parser[R[F[A]]] =
    function2("mapEmpty", eva, eva2)
      .map[R[F[A]]] { case (in, out) => alg.mapEmpty(in)(out) }

  override def mapCons[A, B](eva: Parser[R[F[A]]])(f: Parser[R[S[A] => F[A] => F[B]]]): Parser[R[F[B]]] =
    function2("mapCons", eva, f)
      .map[R[F[B]]] { case (in, out) => alg.mapCons(in)(out) }
}

// Shall we expect Eval already inside R?
class Grammar[S[_], F[_], R[_], Type[_]](
  syntax: CoreDrawingAlgebra[S, F, R],
  self: Expressions[S, F, R, Type],
  orMonoid: MonoidK[Lambda[T => Eval[R[T]]]],
  types: List[Type[_]]
) extends Expressions[S, F, R, Type] {

  override def static[T](t: Type[T]): R[S[T]] = failure

  override def evolution[T](t: Type[T]): R[F[T]] =
    or(
      Eval.later(syntax.empty[T]),
      Eval.later(syntax.cons(self.static(t), self.evolution(t))),
      Eval.later(syntax.mapEmpty(self.evolution(t))(self.evolution(t))),
      Eval.later(or(allMapConsExpressions(t): _*))
    )

  override def mapConsFunction[T1, T2](t1: Type[T1], t2: Type[T2]): R[S[T1] => F[T1] => F[T2]] =
    failure

  private def or[T](expressions: Eval[R[T]]*): R[T] =
    expressions.foldLeft(orMonoid.empty[T])(orMonoid.combineK[T]).value

  private def mapConsExpression[T1, T2](t1: Type[T1], t2: Type[T2]): R[F[T2]] =
    syntax.mapCons(self.evolution(t1))(self.mapConsFunction(t1, t2))

  private def allMapConsExpressions[T](t: Type[T]): Seq[Eval[R[F[T]]]] =
    types.map { s =>
      Eval.later(mapConsExpression(s, t))
    }

  private def failure[T]: R[T] = orMonoid.empty[T].value
}

trait Expressions[S[_], F[_], R[_], Type[_]] {
  def static[T](t: Type[T]): R[S[T]]
  def evolution[T](t: Type[T]): R[F[T]]
  def mapConsFunction[T1, T2](t1: Type[T1], t2: Type[T2]): R[S[T1] => F[T1] => F[T2]]
}

class EmptyExpressions[S[_], F[_], R[_], Type[_]](monoid: MonoidK[R]) extends Expressions[S, F, R, Type] {
  override def static[T](t: Type[T]): R[S[T]] =
    monoid.empty[S[T]]
  override def evolution[T](t: Type[T]): R[F[T]] =
    monoid.empty[F[T]]
  override def mapConsFunction[T1, T2](t1: Type[T1], t2: Type[T2]): R[S[T1] => F[T1] => F[T2]] =
    monoid.empty[S[T1] => F[T1] => F[T2]]
}

object Expressions {
  def fix[S[_], F[_], R[_], Type[_]](
    orMonoid: MonoidK[Lambda[T => Eval[R[T]]]],
    dependentExpressions: Expressions[S, F, R, Type] => Expressions[S, F, R, Type]
  ): Expressions[S, F, R, Type] =
    dependentExpressions(new LazyExpressions(fix[S, F, R, Type](orMonoid, dependentExpressions)))

  def fixMultipleExpressions[S[_], F[_], R[_], Type[_]](
    orMonoid: MonoidK[Lambda[T => Eval[R[T]]]],
    multipleDependentExpressions: List[Expressions[S, F, R, Type] => Expressions[S, F, R, Type]]
  ): Expressions[S, F, R, Type] = {

    def dependentExpressions(expressions: Expressions[S, F, R, Type]): Expressions[S, F, R, Type] =
      OrExpressions[S, F, R, Type](
        orMonoid,
        multipleDependentExpressions.map(dependentExpression => dependentExpression(expressions))
      )

    fix[S, F, R, Type](orMonoid, dependentExpressions)
  }
}

case class OrExpressions[S[_], F[_], R[_], Type[_]](
  orMonoid: MonoidK[Lambda[T => Eval[R[T]]]],
  multipleExpressions: List[Expressions[S, F, R, Type]]
) extends Expressions[S, F, R, Type] {
  override def static[T](t: Type[T]): R[S[T]] =
    combine[S[T]](_.static[T](t))
  override def evolution[T](t: Type[T]): R[F[T]] =
    combine[F[T]](_.evolution[T](t))
  override def mapConsFunction[T1, T2](t1: Type[T1], t2: Type[T2]): R[S[T1] => F[T1] => F[T2]] =
    combine[S[T1] => F[T1] => F[T2]](_.mapConsFunction(t1, t2))

  private def combine[T](f: Expressions[S, F, R, Type] => R[T]): R[T] =
    multipleExpressions
      .map(expressions => Eval.later(f(expressions)))
      .foldLeft(orMonoid.empty[T])(orMonoid.combineK)
      .value
}

class LazyExpressions[S[_], F[_], R[_], Type[_]](expressions: => Expressions[S, F, R, Type])
    extends Expressions[S, F, R, Type] {
  override def static[T](t: Type[T]): R[S[T]] =
    expressions.static(t)
  override def evolution[T](t: Type[T]): R[F[T]] =
    expressions.evolution(t)
  override def mapConsFunction[T1, T2](t1: Type[T1], t2: Type[T2]): R[S[T1] => F[T1] => F[T2]] =
    expressions.mapConsFunction(t1, t2)
}
