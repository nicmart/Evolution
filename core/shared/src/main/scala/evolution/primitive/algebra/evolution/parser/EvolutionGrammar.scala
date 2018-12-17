package evolution.primitive.algebra.evolution.parser

import cats.{ Defer, MonoidK, Semigroup }
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.{ Evolution, EvolutionSyntax, parser }
import evolution.typeclass.VectorSpace
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.primitive.algebra.evolution
import fastparse.noApi.Parser

trait Expressions[F[_], R[_]] {
  def doubleConstant: R[Double]
  def pointConstant: R[Point]
  def evolutionOfDoubles: R[F[Double]]
  def evolutionOfPoints: R[F[Point]]
  def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2]
}

object Expressions {
  class Lazy[F[_], R[_], Var](_inner: => Expressions[F, R], defer: Defer[R]) extends Expressions[F, R] {
    private val inner: Expressions[F, R] = _inner
    def doubleConstant: R[Double] = defer.defer(inner.doubleConstant)
    def pointConstant: R[Point] = defer.defer(inner.pointConstant)
    def evolutionOfDoubles: R[F[Double]] = defer.defer(inner.evolutionOfDoubles)
    def evolutionOfPoints: R[F[Point]] = defer.defer(inner.evolutionOfPoints)
    def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] = defer.defer(inner.function(t1, t2))
  }
}

class EvolutionGrammar[F[_], R[_]](syntax: EvolutionSyntax[F, R], override val orMonoid: MonoidK[R], defer: Defer[R])
    extends Expressions[F, R]
    with OrMonoid[R] {
  import syntax._

  private val self: Expressions[F, R] = new evolution.parser.Expressions.Lazy(this, defer)

  override def doubleConstant: R[Double] =
    or(
      constants.allDoubles,
      constants.sin(self.doubleConstant),
      constants.cos(self.doubleConstant),
      genericConstant(self.doubleConstant))

  override def pointConstant: R[Point] =
    or(
      constants.point(doubleConstant, doubleConstant),
      genericConstant(self.pointConstant)
    )

  override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
    or(
      bind.lambda(bind.allVars, t2),
      genericBinding(self.function(t1, t2))
    )

  override def evolutionOfDoubles: R[F[Double]] =
    or(
      distribution.uniform(doubleConstant, doubleConstant),
      genericEvolution(self.doubleConstant, self.evolutionOfDoubles)
    )

  override def evolutionOfPoints: R[F[Point]] =
    or(
      derived.cartesian(self.evolutionOfDoubles, self.evolutionOfDoubles),
      derived.polar(self.evolutionOfDoubles, self.evolutionOfDoubles),
      genericEvolution(self.pointConstant, self.evolutionOfPoints)
    )

  private def genericConstant[T: VectorSpace](t: R[T]): R[T] =
    or(constants.add(t, t), constants.multiply(self.doubleConstant, t), genericBinding(t))

  private def genericEvolution[T: VectorSpace](t: R[T], ft: R[F[T]]): R[F[T]] =
    or(
      chain.empty,
      chain.cons(t, ft),
      chain.mapCons(ft)(function(t, function(ft, ft))),
      chain.mapEmpty(ft, ft),
      derived.constant(t),
      derived.integrate(t, ft),
      allMappedEvolutions(t),
      genericBinding(ft)
    )

  private def genericBinding[T](t: R[T]): R[T] =
    or(bind.allVarsExpressions, bind.fix(self.function(t, t)), allLetExpressions(t), allAppExpressions(t))

  private def allMappedEvolutions[T](t: R[T]): R[F[T]] =
    or(
      derived.map(self.evolutionOfDoubles, function(self.doubleConstant, t)),
      derived.map(self.evolutionOfPoints, function(self.pointConstant, t))
    )

  private def allLetExpressions[T](t: R[T]): R[T] =
    or(
      bind.let(bind.allVars, self.doubleConstant, t),
      bind.let(bind.allVars, self.pointConstant, t),
      bind.let(bind.allVars, self.evolutionOfDoubles, t),
      bind.let(bind.allVars, self.evolutionOfPoints, t)
    )

  private def allAppExpressions[T](t: R[T]): R[T] =
    or(
      bind.app(self.function(self.doubleConstant, t), self.doubleConstant),
      bind.app(self.function(self.pointConstant, t), self.pointConstant),
      bind.app(self.function(self.evolutionOfDoubles, t), self.evolutionOfDoubles),
      bind.app(self.function(self.evolutionOfPoints, t), self.evolutionOfPoints)
    )
}

object EvolutionGrammar {
  def parserGrammar[F[_], R[_]](alg: Evolution[F, R]): Expressions[F, ByVarParserK[R, ?]] =
    new EvolutionGrammar[F, ByVarParserK[R, ?]](
      new EvolutionParserSyntax[F, R](alg),
      MonoidK[ByVarParserK[R, ?]],
      Defer[ByVarParserK[R, ?]])
}

trait OrMonoid[R[_]] {
  def orMonoid: MonoidK[R]
  // TODO Changed to foldRight to improve generators performance.
  def or[T](expressions: R[T]*): R[T] =
    orSeq(expressions)

  protected def orSeq[T](expressions: Seq[R[T]]): R[T] =
    expressions.foldRight(orMonoid.empty[T])(orMonoid.combineK[T])
}
