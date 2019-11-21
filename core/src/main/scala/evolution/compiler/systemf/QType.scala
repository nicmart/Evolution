package evolution.compiler.systemf

import evolution.compiler.systemf.QType.Arrow
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

sealed trait QType[T] { self: T =>
  def as: T = self
  def =>:[S](from: QType[S]): Arrow[S, T] = QType.Arrow(from, this)
}

object QType {
  final case class Arrow[S, T](from: QType[S], to: QType[T]) extends QType[Arrow[S, T]]
  final case class Simple(qt: Type) extends QType[Simple]
  final case class Forall[T](typeVar: String, tpe: QType[T]) extends QType[Forall[T]]
  final case class Qualified[T](predicate: Predicate, tpe: QType[T]) extends QType[Qualified[T]]

  val asdasd: Arrow[Simple, Simple] = TVar("X") =>: TVar("Y")

  def apply(tpe: Type): QType[Simple] = Simple(tpe)
  def TVar(varname: String): QType[Simple] = QType(Type.Var(varname))
}
