package evolution.compiler.systemf

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified

sealed trait QType {
  def =>:(from: QType): QType = QType.Arrow(from, this)
}

object QType {
  final case class Arrow(from: QType, to: QType) extends QType
  final case class Simple(qt: Qualified[Type]) extends QType
  final case class Forall(typeVar: String, tpe: QType) extends QType

  def apply(tpe: Type): QType = Simple(Qualified(tpe))
  def TVar(varname: String): QType = QType(Type.Var(varname))
}
