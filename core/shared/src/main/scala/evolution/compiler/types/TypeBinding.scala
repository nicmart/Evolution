package evolution.compiler.types

import evolution.compiler.types.TypeClasses.Qualified

sealed abstract class TypeBinding(val name: String, val qualifiedType: Qualified[Type])

object TypeBinding {
  case class Fixed(override val name: String, override val qualifiedType: Qualified[Type])
      extends TypeBinding(name, qualifiedType)
  case class Scheme(override val name: String, override val qualifiedType: Qualified[Type])
      extends TypeBinding(name, qualifiedType)
}
