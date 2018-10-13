package evolution.primitive.algebra.evolution.parser

import cats.{Defer, MonoidK, Semigroup}
import evolution.geometry.Point
import evolution.primitive.algebra.{ByVarParser, Composed}
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import cats.instances.double._
import evolution.primitive.algebra.binding.BindingAlgebra
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.list.{ContextualListAlgebra, ListAlgebra}
import evolution.primitive.algebra.parser.PrimitiveParsers
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi.{Fail, P, Parser}
import Instances._
import evolution.primitive.algebra.list.parser.ListAlgebraSyntax

trait EvolutionAlgebraExpressions[S[_], F[_], R[_]] {
  def list: ListAlgebraExpressions[S, F, R]
  def constants: ConstantsAlgebraExpressions[S, R]
  def binding: BindingAlgebraExpressions[R]
}

object EvolutionAlgebraExpressions {
  class Lazy[S[_], F[_], R[_]](inner: => EvolutionAlgebraExpressions[S, F, R], defer: Defer[R])
      extends EvolutionAlgebraExpressions[S, F, R] {
    override def list: ListAlgebraExpressions[S, F, R] =
      new ListAlgebraExpressions.Lazy(inner.list, defer)
    override def constants: ConstantsAlgebraExpressions[S, R] =
      new ConstantsAlgebraExpressions.Lazy(inner.constants, defer)
    override def binding: BindingAlgebraExpressions[R] =
      new BindingAlgebraExpressions.Lazy(inner.binding, defer)
  }
}

trait ListAlgebraExpressions[S[_], F[_], R[_]] {
  def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]]
}

object ListAlgebraExpressions {
  class Lazy[S[_], F[_], R[_]](inner: => ListAlgebraExpressions[S, F, R], defer: Defer[R])
      extends ListAlgebraExpressions[S, F, R] {
    override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] = defer.defer(inner.evolutionOf(constant))
  }
}

class ListAlgebraEvolutionGrammar[S[_], F[_], R[_]](
  self: EvolutionAlgebraExpressions[S, F, R],
  syntax: ListAlgebra[S, F, R],
  override val orMonoid: MonoidK[R],
) extends ListAlgebraExpressions[S, F, R]
    with OrMonoid[R] {
  import syntax._
  override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] =
    or(
      empty,
      cons(self.constants.constantOf(constant), self.list.evolutionOf(constant)),
      mapCons(self.list.evolutionOf(constant))(
        self.binding.function(
          self.constants.constantOf(constant),
          self.binding.function(self.list.evolutionOf(constant), self.list.evolutionOf(constant))
        )
      ),
      mapEmpty(self.list.evolutionOf(constant))(self.list.evolutionOf(constant))
    )
}

trait ConstantsAlgebraExpressions[S[_], R[_]] {
  def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]]
  def points: R[S[Point]]
  def doubles: R[S[Double]]
}

object ConstantsAlgebraExpressions {
  class Lazy[S[_], R[_]](inner: => ConstantsAlgebraExpressions[S, R], defer: Defer[R])
      extends ConstantsAlgebraExpressions[S, R] {
    override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
      defer.defer(inner.constantOf(constant))
    override def points: R[S[Point]] = defer.defer(inner.points)
    override def doubles: R[S[Double]] = defer.defer(inner.doubles)
  }
}

class ConstantsAlgebraGrammar[S[_], R[_]](
  self: ConstantsAlgebraExpressions[S, R],
  syntax: ConstantsAlgebra[Composed[R, S, ?]],
  doubleLiteral: R[S[Double]],
  override val orMonoid: MonoidK[R]
) extends ConstantsAlgebraExpressions[S, R]
    with OrMonoid[R] {
  import syntax._
  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
    or(constant, add(self.constantOf(constant), self.constantOf(constant)))
  override def points: R[S[Point]] =
    point(self.constantOf(doubles), self.constantOf(doubles))
  override def doubles: R[S[Double]] =
    doubleLiteral
}

trait BindingAlgebraExpressions[R[_]] {
  def valueOf[T](t: R[T]): R[T]
  def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2]
}

object BindingAlgebraExpressions {
  class Lazy[R[_]](inner: => BindingAlgebraExpressions[R], defer: Defer[R]) extends BindingAlgebraExpressions[R] {
    override def valueOf[T](t: R[T]): R[T] = defer.defer(inner.valueOf(t))
    override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] = defer.defer(inner.function(t1, t2))
  }
}

class BindingAlgebraGrammar[R[_], VarName](
  self: BindingAlgebraExpressions[R],
  syntax: BindingAlgebra[R, VarName],
  variableSyntax: VarName,
  all: List[R[_]],
  override val orMonoid: MonoidK[R]
) extends BindingAlgebraExpressions[R]
    with OrMonoid[R] {
  import syntax._

  // TODO allLet and allApp break Generators
  override def valueOf[T](t: R[T]): R[T] =
    or(var0, shift(self.valueOf(t)), fix(self.function(t, t)), allLetExpressions(t), allAppExpressions(t))

  override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
    or(lambda(variableSyntax, t2), valueOf(self.function(t1, t2)))

  private def letExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
    let(variableSyntax, self.valueOf(t1))(self.valueOf(t2))

  private def allLetExpressions[T](t: R[T]): R[T] =
    or(all.map(s => letExpression(s, t)): _*)

  private def appExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
    app(self.function(t1, t2), self.valueOf(t1))

  private def allAppExpressions[T](t: R[T]): R[T] =
    or(all.map(s => appExpression(self.valueOf(s), t)): _*)
}

trait OrMonoid[R[_]] {
  def orMonoid: MonoidK[R]
  // TODO Changed to foldRight to improve generators performance.
  def or[T](expressions: R[T]*): R[T] =
    orSeq(expressions)

  protected def orSeq[T](expressions: Seq[R[T]]): R[T] =
    expressions.foldRight(orMonoid.empty[T])(orMonoid.combineK[T])
}

class EvolutionAlgebraGrammar[S[_], F[_], R[_], VarName](
  self: EvolutionAlgebraExpressions[S, F, R],
  syntax: EvolutionAlgebra[S, F, R, VarName],
  doubleLiteral: R[S[Double]],
  variableSyntax: VarName,
  override val orMonoid: MonoidK[R]
) extends EvolutionAlgebraExpressions[S, F, R]
    with OrMonoid[R] {

  override def list: ListAlgebraExpressions[S, F, R] = new ListAlgebraExpressions[S, F, R] {
    override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] =
      or(internalList.evolutionOf(constant), internalBinding.valueOf(self.list.evolutionOf(constant)))
  }

  override def constants: ConstantsAlgebraExpressions[S, R] =
    new ConstantsAlgebraExpressions[S, R] {
      override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
        or(internalConstants.constantOf(constant), internalBinding.valueOf(self.constants.constantOf(constant)))
      override def points: R[S[Point]] =
        internalConstants.points
      override def doubles: R[S[Double]] =
        internalConstants.doubles
    }

  override def binding: BindingAlgebraExpressions[R] = new BindingAlgebraExpressions[R] {
    override def valueOf[T](t: R[T]): R[T] =
      or(t, internalBinding.valueOf(self.binding.valueOf(t)))
    override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
      internalBinding.function(t1, t2)
  }

  private lazy val doubles = internalConstants.doubles
  private lazy val points = internalConstants.points

  private lazy val all: List[R[_]] = List[R[_]](
    self.constants.constantOf(doubles),
    self.constants.constantOf(points),
    self.list.evolutionOf(doubles),
    self.list.evolutionOf(points)
  )

  private lazy val internalList: ListAlgebraExpressions[S, F, R] =
    new ListAlgebraEvolutionGrammar(self, syntax.list, orMonoid)

  private lazy val internalConstants: ConstantsAlgebraExpressions[S, R] =
    new ConstantsAlgebraGrammar[S, R](self.constants, syntax.constants, doubleLiteral, orMonoid)

  private lazy val internalBinding: BindingAlgebraExpressions[R] =
    new BindingAlgebraGrammar(self.binding, syntax.bind, variableSyntax, all, orMonoid)
}

object EvolutionAlgebraGrammar {
  import EvolutionAlgebraExpressions.Lazy, Instances._

  def grammar[S[_], F[_], R[_]](
    alg: EvolutionAlgebra[S, F, R, String]
  ): EvolutionAlgebraExpressions[S, F, ByVarParser[R, ?]] = {
    parserGrammarRec[S, F, R](alg, new Lazy[S, F, ByVarParser[R, ?]](grammar(alg), defer[R]))
  }

  private def parserGrammarRec[S[_], F[_], R[_]](
    alg: EvolutionAlgebra[S, F, R, String],
    self: EvolutionAlgebraExpressions[S, F, ByVarParser[R, ?]]
  ): EvolutionAlgebraExpressions[S, F, ByVarParser[R, ?]] = {
    val syntax = new EvolutionAlgebraSyntax[S, F, R](alg)
    new EvolutionAlgebraGrammar[S, F, ByVarParser[R, ?], Parser[String]](
      self,
      syntax,
      syntax.doubleConstant,
      PrimitiveParsers.varName,
      orMonoid[R]
    )
  }
}

object Instances {
  def orMonoid[R[_]]: MonoidK[ByVarParser[R, ?]] = new MonoidK[ByVarParser[R, ?]] {
    override def empty[A]: ByVarParser[R, A] =
      _ => Fail
    override def combineK[A](x: ByVarParser[R, A], y: ByVarParser[R, A]): ByVarParser[R, A] =
      ctx => P(x(ctx) | y(ctx))
  }

  def defer[R[_]]: Defer[ByVarParser[R, ?]] = new Defer[ByVarParser[R, ?]] {
    override def defer[A](fa: => ByVarParser[R, A]): ByVarParser[R, A] =
      ctx => P(fa(ctx))
  }
}
