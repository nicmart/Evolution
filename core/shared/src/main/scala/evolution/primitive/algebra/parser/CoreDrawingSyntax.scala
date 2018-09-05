package evolution.primitive.algebra.parser

import cats.{Eval, MonoidK}
import evolution.primitive.algebra.CoreDrawingAlgebra
import ParserConfig.White._
import PrimitiveParsers._
import fastparse.noApi._

class CoreDrawingSyntax[S[_], F[_], R[_]](alg: CoreDrawingAlgebra[S, F, R])
    extends CoreDrawingAlgebra[S, F, λ[α => Parser[R[α]]]] {

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

class LazyCoreDrawingAlgebra[S[_], F[_], R[_]](alg: CoreDrawingAlgebra[S, F, R])
    extends CoreDrawingAlgebra[S, F, λ[α => Eval[R[α]]]] {

  override def empty[A]: Eval[R[F[A]]] =
    Eval.later(alg.empty)

  override def cons[A](head: Eval[R[S[A]]], tail: Eval[R[F[A]]]): Eval[R[F[A]]] =
    Eval.later(alg.cons(head.value, tail.value))

  override def mapEmpty[A](eva: Eval[R[F[A]]])(eva2: Eval[R[F[A]]]): Eval[R[F[A]]] =
    Eval.later(alg.mapEmpty(eva.value)(eva2.value))

  override def mapCons[A, B](eva: Eval[R[F[A]]])(f: Eval[R[S[A] => F[A] => F[B]]]): Eval[R[F[B]]] =
    Eval.later(alg.mapCons(eva.value)(f.value))
}

// Shall we expect Eval already inside R?
class Grammar[S[_], F[_], R[_], Type[_]](
  self: => Expressions[S, F, R, Type],
  syntax: CoreDrawingAlgebra[S, F, R],
  orMonoid: MonoidK[R],
  types: List[Type[_]]
) extends Expressions[S, F, R, Type] {

  override def static[T](t: Type[T]): R[S[T]] = failure

  override def evolution[T](t: Type[T]): R[F[T]] =
    or(
      syntax.empty[T],
      syntax.cons(self.static(t), self.evolution(t)),
      syntax.mapEmpty(self.evolution(t))(self.evolution(t)),
      or(allMapConsExpressions(t): _*)
    )

  override def mapConsFunction[T1, T2](t1: Type[T1], t2: Type[T2]): R[S[T1] => F[T1] => F[T2]] =
    failure

  private def or[T](expressions: R[T]*): R[T] =
    expressions.foldLeft(orMonoid.empty[T])(orMonoid.combineK[T])

  private def mapConsExpression[T1, T2](t1: Type[T1], t2: Type[T2]): R[F[T2]] =
    syntax.mapCons(self.evolution(t1))(self.mapConsFunction(t1, t2))

  private def allMapConsExpressions[T](t: Type[T]): Seq[R[F[T]]] =
    types.map(s => mapConsExpression(s, t))

  private def failure[T]: R[T] = orMonoid.empty[T]
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
    dependentExpressions: Eval[Expressions[S, F, R, Type]] => Expressions[S, F, R, Type]
  ): Expressions[S, F, R, Type] =
    dependentExpressions(Eval.later(fix[S, F, R, Type](dependentExpressions)))

  def fixMultipleExpressions[S[_], F[_], R[_], Type[_]](
    orMonoid: MonoidK[R],
    multipleDependentExpressions: List[Eval[Expressions[S, F, R, Type]] => Expressions[S, F, R, Type]]
  ): Expressions[S, F, R, Type] = {

    def dependentExpressions(expressions: Eval[Expressions[S, F, R, Type]]): Expressions[S, F, R, Type] =
      OrExpressions[S, F, R, Type](
        orMonoid,
        multipleDependentExpressions.map(dependentExpression => dependentExpression(expressions))
      )

    fix[S, F, R, Type](dependentExpressions)
  }
}

case class OrExpressions[S[_], F[_], R[_], Type[_]](
  orMonoid: MonoidK[R],
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
      .map(f)
      .foldLeft(orMonoid.empty[T])(orMonoid.combineK)
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
