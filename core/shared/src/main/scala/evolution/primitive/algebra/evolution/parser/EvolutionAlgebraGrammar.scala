package evolution.primitive.algebra.evolution.parser
import cats.{Defer, MonoidK, Semigroup}
import evolution.geometry.Point
import evolution.primitive.algebra.ByVarParser
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import cats.instances.double._

trait EvolutionAlgebraExpressions[S[_], F[_], R[_], VarName] {
  def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]]
  def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]]
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

  override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] =
    or(empty, cons(self.constantOf(constant), self.evolutionOf(constant)))

  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] =
    or(constant, add(self.constantOf(constant), self.constantOf(constant)))

  override def points: R[S[Point]] =
    point(self.constantOf(doubles), self.constantOf(doubles))

  override def doubles: R[S[Double]] =
    doubleLiteral

//  override def value[T](t: R[T]): R[T] =
//    or(t, valueRec(self.value(t)))
//
//  override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
//    or(lambda(variableSyntax, t2), valueRec(self.function(t1, t2)))

  private val all: List[R[_]] =
    List[R[_]](self.constantOf(doubles), self.constantOf(points), self.evolutionOf(doubles), self.evolutionOf(points))

//  private def valueRec[T](t: R[T]): R[T] =
//    or(var0, shift(t), fix(self.function(t, t)), allLetExpressions(t), allAppExpressions(t))

//  private def letExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
//    syntax.let(varNameSyntax, self.value(t1))(t2)
//
//  private def allLetExpressions[T](t: R[T]): R[T] =
//    or(all.map(s => letExpression(s, t)): _*)
//
//  private def appExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
//    app(self.function(t1, t2), t1)
//
//  private def allAppExpressions[T](t: R[T]): R[T] =
//    or(all.map(s => appExpression(self.value(s), t)): _*)

  private def or[T](expressions: R[T]*): R[T] = expressions.fold(orMonoid.empty[T])(orMonoid.combineK[T])
}

class LazyEvolutionAlgebraExpressions[S[_], F[_], R[_], VarName](
  inner: => EvolutionAlgebraExpressions[S, F, R, VarName],
  defer: Defer[R]
) extends EvolutionAlgebraExpressions[S, F, R, VarName] {
  override def evolutionOf[T: Semigroup](constant: R[S[T]]): R[F[T]] = defer.defer(inner.evolutionOf(constant))
  override def constantOf[T: Semigroup](constant: R[S[T]]): R[S[T]] = defer.defer(inner.constantOf(constant))
  override def doubles: R[S[Double]] = defer.defer(inner.doubles)
  override def points: R[S[Point]] = defer.defer(inner.points)
}
