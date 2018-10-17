package evolution.primitive.algebra.evolution.parser

import cats.{Defer, MonoidK, Semigroup}
import evolution.geometry.Point
import evolution.primitive.algebra.{ByVarParser, Composed}
import evolution.primitive.algebra.evolution.Evolution
import cats.instances.double._
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.chain.{ContextualChain, Chain}
import evolution.primitive.algebra.parser.PrimitiveParsers
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi.{Fail, P, Parser}
import Instances._

trait EvolutionExpressions[S[_], F[_], R[_]] {
  def list: ChainExpressions[S, F, R]
  def constants: ConstantsExpressions[S, R]
  def binding: BindingExpressions[R]
}

object EvolutionExpressions {
  class Lazy[S[_], F[_], R[_]](inner: => EvolutionExpressions[S, F, R], defer: Defer[R])
      extends EvolutionExpressions[S, F, R] {
    override def list: ChainExpressions[S, F, R] =
      new ChainExpressions.Lazy(inner.list, defer)
    override def constants: ConstantsExpressions[S, R] =
      new ConstantsExpressions.Lazy(inner.constants, defer)
    override def binding: BindingExpressions[R] =
      new BindingExpressions.Lazy(inner.binding, defer)
  }
}

trait ChainExpressions[S[_], F[_], R[_]] {
  def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]]
}

object ChainExpressions {
  class Lazy[S[_], F[_], R[_]](inner: => ChainExpressions[S, F, R], defer: Defer[R]) extends ChainExpressions[S, F, R] {
    override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] = defer.defer(inner.evolutionOf(constant))
  }
}

class ChainGrammar[S[_], F[_], R[_]](
  self: EvolutionExpressions[S, F, R],
  syntax: Chain[S, F, R],
  override val orMonoid: MonoidK[R],
) extends ChainExpressions[S, F, R]
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

trait ConstantsExpressions[S[_], R[_]] {
  def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]]
  def points: R[S[Point]]
  def doubles: R[S[Double]]
}

object ConstantsExpressions {
  class Lazy[S[_], R[_]](inner: => ConstantsExpressions[S, R], defer: Defer[R]) extends ConstantsExpressions[S, R] {
    override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
      defer.defer(inner.constantOf(constant))
    override def points: R[S[Point]] = defer.defer(inner.points)
    override def doubles: R[S[Double]] = defer.defer(inner.doubles)
  }
}

class ConstantsGrammar[S[_], R[_]](
  self: ConstantsExpressions[S, R],
  syntax: Constants[Composed[R, S, ?], Unit],
  override val orMonoid: MonoidK[R]
) extends ConstantsExpressions[S, R]
    with OrMonoid[R] {
  import syntax._
  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
    or(constant, add(self.constantOf(constant), self.constantOf(constant)))
  override def points: R[S[Point]] =
    point(self.constantOf(doubles), self.constantOf(doubles))
  override def doubles: R[S[Double]] =
    syntax.double(())
}

trait BindingExpressions[R[_]] {
  def valueOf[T](t: R[T]): R[T]
  def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2]
}

object BindingExpressions {
  class Lazy[R[_]](inner: => BindingExpressions[R], defer: Defer[R]) extends BindingExpressions[R] {
    override def valueOf[T](t: R[T]): R[T] = defer.defer(inner.valueOf(t))
    override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] = defer.defer(inner.function(t1, t2))
  }
}

class BindingGrammar[R[_], Var](
  self: BindingExpressions[R],
  syntax: Binding[R, Var],
  variableSyntax: Var,
  all: List[R[_]],
  override val orMonoid: MonoidK[R]
) extends BindingExpressions[R]
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

class EvolutionGrammar[S[_], F[_], R[_], Var](
  self: EvolutionExpressions[S, F, R],
  syntax: Evolution[S, F, R, Unit, Var],
  variableSyntax: Var,
  override val orMonoid: MonoidK[R]
) extends EvolutionExpressions[S, F, R]
    with OrMonoid[R] {

  private def doubleLiteral: Composed[R, S, Double] = syntax.constants.double(())

  override def list: ChainExpressions[S, F, R] = new ChainExpressions[S, F, R] {
    override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] =
      or(internalList.evolutionOf(constant), internalBinding.valueOf(self.list.evolutionOf(constant)))
  }

  override def constants: ConstantsExpressions[S, R] =
    new ConstantsExpressions[S, R] {
      override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
        or(internalConstants.constantOf(constant), internalBinding.valueOf(self.constants.constantOf(constant)))
      override def points: R[S[Point]] =
        internalConstants.points
      override def doubles: R[S[Double]] =
        internalConstants.doubles
    }

  override def binding: BindingExpressions[R] = new BindingExpressions[R] {
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

  private lazy val internalList: ChainExpressions[S, F, R] =
    new ChainGrammar(self, syntax.list, orMonoid)

  private lazy val internalConstants: ConstantsExpressions[S, R] =
    new ConstantsGrammar[S, R](self.constants, syntax.constants, orMonoid)

  private lazy val internalBinding: BindingExpressions[R] =
    new BindingGrammar(self.binding, syntax.bind, variableSyntax, all, orMonoid)
}

object EvolutionGrammar {
  import EvolutionExpressions.Lazy, Instances._

  def grammar[S[_], F[_], R[_]](
    alg: Evolution[S, F, R, Double, String]
  ): EvolutionExpressions[S, F, ByVarParser[R, ?]] = {
    parserGrammarRec[S, F, R](alg, new Lazy[S, F, ByVarParser[R, ?]](grammar(alg), defer[R]))
  }

  private def parserGrammarRec[S[_], F[_], R[_]](
    alg: Evolution[S, F, R, Double, String],
    self: EvolutionExpressions[S, F, ByVarParser[R, ?]]
  ): EvolutionExpressions[S, F, ByVarParser[R, ?]] = {
    val syntax = new EvolutionSyntax[S, F, R](alg)
    new EvolutionGrammar[S, F, ByVarParser[R, ?], Parser[String]](self, syntax, PrimitiveParsers.varName, orMonoid[R])
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
