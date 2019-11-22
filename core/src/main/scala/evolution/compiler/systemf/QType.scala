package evolution.compiler.systemf

import evolution.compiler.phases.typer.model.{CanBeSubstituted, Substitution}
import evolution.compiler.systemf.QType.Arrow
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

sealed trait QType[T] { self: T =>
  def as: T = self
  def =>:[S](from: QType[S]): Arrow[S, T] = QType.Arrow(from, this)
  def substitute(substitution: Substitution): QType[T] // Not able to implement this with Pattern Matching without doing casts
}

object QType {

  type =>:[S, T] = Arrow[S, T]
  type * = Simple

  final case class Simple(qt: Type) extends QType[Simple] {
    def substitute(substitution: Substitution): QType[Simple] = this
  }

  final case class Arrow[S, T](from: QType[S], to: QType[T]) extends QType[S =>: T] {
    override def substitute(substitution: Substitution): QType[S =>: T] =
      Arrow(from.substitute(substitution), to.substitute(substitution))
  }

  final case class Forall[T](typeVar: String, tpe: QType[T]) extends QType[Forall[T]] {
    override def substitute(substitution: Substitution): QType[Forall[T]] =
      Forall(typeVar, tpe.substitute(substitution.without(typeVar)))
  }

  final case class Qualified[T](predicate: Predicate, tpe: QType[T]) extends QType[Qualified[T]] {
    override def substitute(substitution: Substitution): QType[Qualified[T]] =
      Qualified(substitution.substitute(predicate), tpe.substitute(substitution))
  }

  def apply(tpe: Type): QType[Simple] = Simple(tpe)
  def TVar(varname: String): QType[Simple] = QType(Type.Var(varname))

  implicit def canBeSubstituted[T]: CanBeSubstituted[QType[T]] = (s: Substitution, qt: QType[T]) => qt.substitute(s)
}
