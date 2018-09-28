package evolution.primitive.algebra.evolution.parser
import cats.{Defer, MonoidK, Semigroup}
import evolution.geometry.Point
import evolution.primitive.algebra.ByVarParser
import evolution.primitive.algebra.evolution.EvolutionAlgebra

trait EvolutionAlgebraExpressions[S[_], F[_], R[_], VarName] {
  def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]]
  def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]]
}

class EvolutionAlgebraGrammar[S[_], F[_], R[_], VarName](
  self: EvolutionAlgebraExpressions[S, F, R, VarName],
  syntax: EvolutionAlgebra[S, F, R, VarName],
  orMonoid: MonoidK[R]
) extends EvolutionAlgebraExpressions[S, F, R, VarName] {
  import syntax.list._, syntax.bind._, syntax.constants._

  override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] =
    or(empty, cons(self.constantOf(constant), self.evolutionOf(constant)))

  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
    or(constant, add(self.constantOf(constant), self.constantOf(constant)))

  private def or[T](expressions: R[T]*): R[T] = expressions.fold(orMonoid.empty[T])(orMonoid.combineK[T])
}

class LazyEvolutionAlgebraExpressions[S[_], F[_], R[_], VarName](
  inner: => EvolutionAlgebraExpressions[S, F, R, VarName],
  defer: Defer[R]
) extends EvolutionAlgebraExpressions[S, F, R, VarName] {
  override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] = defer.defer(inner.evolutionOf(constant))
  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] = defer.defer(inner.constantOf(constant))
}
