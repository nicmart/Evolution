package evolution.compiler.types

import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type.Scheme

final case class Assumption(val name: String, val qualifiedScheme: Qualified[Scheme], primitive: Boolean)
