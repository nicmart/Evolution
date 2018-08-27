package evolution.primitive.algebra.parser

import cats.{Eval, MonoidK, SemigroupK}
import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser.ExperimentalCoreDrawingAlgebraParser.Compose
import evolution.primitive.algebra.parser.ParsersContainerOps._
import ParserConfig.White._
import PrimitiveParsers._
import fastparse.noApi
import fastparse.noApi._

import scala.collection.immutable

class ExperimentalCoreDrawingAlgebraParser[S[_], F[_], R[_]](alg: CoreDrawingAlgebra[S, F, R])
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

object ExperimentalCoreDrawingAlgebraParser {
  type Compose[A[_], B[_], T] = A[B[T]]

  def monoidK[R[_]]: MonoidK[Lambda[T => Eval[Parser[R[T]]]]] = new MonoidK[Lambda[T => Eval[Parser[R[T]]]]] {
    override def empty[A]: Eval[Parser[R[A]]] = Eval.now(Fail)
    override def combineK[A](x: Eval[Parser[R[A]]], y: Eval[Parser[R[A]]]): Eval[Parser[R[A]]] =
      Eval.now(P(x.value | y.value))
  }

  def or[R[_], T](rs: List[Eval[Parser[R[T]]]]): Parser[R[T]] = rs match {
    case Nil => Fail
    case head :: tail => P(head.value | or(tail))
  }
}

trait Expressions[S[_], F[_], R[_]] {
  type T

  def exprF(self: Expressions.Aux[S, F, R, T], all: List[Expressions[S, F, R]]): R[F[T]]
  def exprS(self: Expressions.Aux[S, F, R, T], all: List[Expressions[S, F, R]]): R[S[T]]
  def exprFunc(self: Expressions.Aux[S, F, R, T], other: Expressions[S, F, R]): R[S[other.T] => F[other.T] => F[T]]
}

object Expressions {
  type Aux[S[_], F[_], R[_], U] = Expressions[S, F, R] { type T = U }
  def empty[S[_], F[_], R[_]: MonoidK, X]: Expressions.Aux[S, F, R, X] = new Expressions[S, F, R] {
    type T = X
    private val monoidK: MonoidK[R] = MonoidK[R]
    override def exprF(self: Aux[S, F, R, T], all: List[Expressions[S, F, R]]): R[F[T]] = monoidK.algebra[F[T]].empty
    override def exprS(self: Aux[S, F, R, T], all: List[Expressions[S, F, R]]): R[S[T]] = monoidK.algebra[S[T]].empty
    override def exprFunc(self: Aux[S, F, R, T], other: Expressions[S, F, R]): R[S[other.T] => F[other.T] => F[T]] =
      monoidK.algebra[S[other.T] => F[other.T] => F[T]].empty
  }
}

class Grammar[S[_], F[_], R[_], X](alg: CoreDrawingAlgebra[S, F, R], monoidK: MonoidK[Lambda[T => Eval[R[T]]]])
    extends Expressions[S, F, R] {
  import Eval.later
  final type T = X

  override def exprF(self: Expressions.Aux[S, F, R, T], all: List[Expressions[S, F, R]]): R[F[T]] =
    or[F[T]](
      List(
        later(self.exprF(self, all)),
        later(alg.empty),
        later(alg.cons(exprS(self, all), exprF(self, all))),
        later(alg.mapEmpty(exprF(self, all))(exprF(self, all))),
        later(allMapConsExpressions(self, all))
      )
    ).value

  override def exprS(self: Expressions.Aux[S, F, R, T], all: List[Expressions[S, F, R]]): R[S[T]] =
    self.exprS(self, all)

  override def exprFunc(
    self: Expressions.Aux[S, F, R, T],
    other: Expressions[S, F, R]
  ): R[S[other.T] => F[other.T] => F[T]] =
    self.exprFunc(self, other)

  private def allMapConsExpressions(self: Expressions.Aux[S, F, R, T], all: List[Expressions[S, F, R]]): R[F[T]] = {
    val allMapConsExpressions = all.map { otherExpressions =>
      val exprFunction = exprFunc(self, otherExpressions)
      later(alg.mapCons[otherExpressions.T, T](otherExpressions.exprF(otherExpressions, all))(exprFunction))
    }

    or(allMapConsExpressions).value
  }

  private def or[T](expressions: List[Eval[R[T]]]): Eval[R[T]] = expressions match {
    case Nil => monoidK.empty[T]
    case head :: tail => monoidK.combineK(head, or(tail))
  }
}
