package evolution.primitive.algebra.evolution.parser
import cats.{Defer, MonoidK, Semigroup}
import evolution.geometry.Point
import evolution.primitive.algebra.ByVarParser
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import cats.instances.double._

trait EvolutionAlgebraExpressions[S[_], F[_], R[_], VarName] {
  def evolutionOfPoints: R[F[Point]]
  def evolutionOfDoubles: R[F[Double]]
  def pointConstant: R[S[Point]]
  def doubleConstant: R[S[Double]]
}

class EvolutionAlgebraGrammar[S[_], F[_], R[_], VarName](
  self: EvolutionAlgebraExpressions[S, F, R, VarName],
  syntax: EvolutionAlgebra[S, F, R, VarName],
  orMonoid: MonoidK[R],
  doubleLiteral: R[S[Double]]
) extends EvolutionAlgebraExpressions[S, F, R, VarName] {
  import syntax.list._, syntax.bind._, syntax.constants._

  override def evolutionOfPoints: R[F[Point]] =
    or(empty, cons(self.pointConstant, self.evolutionOfPoints))
  override def evolutionOfDoubles: R[F[Double]] =
    or(empty, cons(self.doubleConstant, self.evolutionOfDoubles))
  override def pointConstant: R[S[Point]] =
    or(point(self.doubleConstant, self.doubleConstant), add(self.pointConstant, self.pointConstant))
  override def doubleConstant: R[S[Double]] =
    or(doubleLiteral, add(self.doubleConstant, self.doubleConstant))

  private def or[T](expressions: R[T]*): R[T] = expressions.fold(orMonoid.empty[T])(orMonoid.combineK[T])
}

class LazyEvolutionAlgebraExpressions[S[_], F[_], R[_], VarName](
  inner: => EvolutionAlgebraExpressions[S, F, R, VarName],
  defer: Defer[R]
) extends EvolutionAlgebraExpressions[S, F, R, VarName] {
  override def evolutionOfPoints: R[F[Point]] = defer.defer(inner.evolutionOfPoints)
  override def evolutionOfDoubles: R[F[Double]] = defer.defer(inner.evolutionOfDoubles)
  override def pointConstant: R[S[Point]] = defer.defer(inner.pointConstant)
  override def doubleConstant: R[S[Double]] = defer.defer(inner.doubleConstant)
}
