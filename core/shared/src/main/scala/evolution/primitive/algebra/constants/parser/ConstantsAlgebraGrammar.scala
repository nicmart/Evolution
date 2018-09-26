package evolution.primitive.algebra.constants.parser
import cats.{Defer, MonoidK}
import cats.kernel.Semigroup
import evolution.primitive.algebra.constants.ConstantsAlgebra

class ConstantsAlgebraGrammar[S[_]](self: Expressions[S], syntax: ConstantsAlgebra[S], orMonoid: MonoidK[S])
    extends Expressions[S] {
  override def get[T: Semigroup](t: S[T]): S[T] =
    or(t, syntax.add(self.get(t), self.get(t)))

  private def or[T](expressions: S[T]*): S[T] =
    expressions.foldLeft(orMonoid.empty[T])(orMonoid.combineK[T])
}

trait Expressions[S[_]] {
  def get[T: Semigroup](self: S[T]): S[T]
}

class LazyExpressions[S[_]](expressions: => Expressions[S], defer: Defer[S]) extends Expressions[S] {
  override def get[T: Semigroup](self: S[T]): S[T] = defer.defer(expressions.get(self))
}
