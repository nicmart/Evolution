package evolution.primitive.algebra.evolution.parser
import cats.{Defer, MonoidK, Semigroup}
import evolution.geometry.Point
import evolution.primitive.algebra.ByVarParser
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import cats.instances.double._

trait EvolutionAlgebraExpressions[S[_], F[_], R[_], VarName] {
  def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]]
  def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]]
  def valueOf[T](t: R[T]): R[T]
  def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2]
  def doubles: R[S[Double]]
  def points: R[S[Point]]
}

class EvolutionAlgebraGrammar[S[_], F[_], R[_], VarName](
  self: EvolutionAlgebraExpressions[S, F, R, VarName],
  syntax: EvolutionAlgebra[S, F, R, VarName],
  orMonoid: MonoidK[R],
  doubleLiteral: R[S[Double]],
  variableSyntax: VarName
) extends EvolutionAlgebraExpressions[S, F, R, VarName] {
  import syntax.list._, syntax.bind._, syntax.constants._

  // TODO refactor a bit, we need a better understanding of what is going on
  override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] =
    or(
      empty,
      cons(self.constantOf(constant), self.evolutionOf(constant)),
      mapCons(self.evolutionOf(constant))(
        self.function(self.constantOf(constant), self.function(self.evolutionOf(constant), self.evolutionOf(constant)))
      ),
      mapEmpty(self.evolutionOf(constant))(self.evolutionOf(constant)),
      valueRec(self.evolutionOf(constant))
    )

  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
    or(constant, add(self.constantOf(constant), self.constantOf(constant)), valueRec(self.constantOf(constant)))

  override def points: R[S[Point]] =
    point(self.constantOf(doubles), self.constantOf(doubles))

  override def doubles: R[S[Double]] =
    doubleLiteral

  override def valueOf[T](t: R[T]): R[T] =
    or(t, valueRec(self.valueOf(t)))

  override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
    or(lambda(variableSyntax, t2), valueRec(self.function(t1, t2)))

  private val all: List[R[_]] =
    List[R[_]](self.constantOf(doubles), self.constantOf(points), self.evolutionOf(doubles), self.evolutionOf(points))

  private def valueRec[T](t: R[T]): R[T] =
    or(var0, shift(t), fix(self.function(t, t)), allLetExpressions(t), allAppExpressions(t))

  private def letExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
    let(variableSyntax, self.valueOf(t1))(t2)

  private def allLetExpressions[T](t: R[T]): R[T] =
    or(all.map(s => letExpression(s, t)): _*)

  private def appExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
    app(self.function(t1, t2), t1)

  private def allAppExpressions[T](t: R[T]): R[T] =
    or(all.map(s => appExpression(self.valueOf(s), t)): _*)

  private def or[T](expressions: R[T]*): R[T] = expressions.fold(orMonoid.empty[T])(orMonoid.combineK[T])
}

class LazyEvolutionAlgebraExpressions[S[_], F[_], R[_], VarName](
  inner: => EvolutionAlgebraExpressions[S, F, R, VarName],
  defer: Defer[R]
) extends EvolutionAlgebraExpressions[S, F, R, VarName] {
  override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] = defer.defer(inner.evolutionOf(constant))
  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] = defer.defer(inner.constantOf(constant))
  override def valueOf[T](value: R[T]): R[T] = defer.defer(inner.valueOf(value))
  override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] = defer.defer(inner.function(t1, t2))
  override def doubles: R[S[Double]] = defer.defer(inner.doubles)
  override def points: R[S[Point]] = defer.defer(inner.points)
}
