package evolution.compiler.types

import evolution.compiler.types.TypeClasses.Qualified

final case class TypeBinding(val name: String, val qualifiedType: Qualified[Type], primitive: Boolean)
