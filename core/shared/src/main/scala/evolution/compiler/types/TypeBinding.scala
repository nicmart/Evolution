package evolution.compiler.types

import evolution.compiler.types.TypeClasses.Qualified

sealed abstract class TypeBinding(val name: String, val qt: Qualified[Type])

object TypeBinding {
  case class Variable(override val name: String, override val qt: Qualified[Type]) extends TypeBinding(name, qt)
  case class Predefined(override val name: String, override val qt: Qualified[Type]) extends TypeBinding(name, qt)
}
