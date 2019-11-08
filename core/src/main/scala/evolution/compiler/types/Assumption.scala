package evolution.compiler.types

import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type.Scheme

final case class Assumption(name: String, qualifiedScheme: Qualified[Scheme], primitive: Boolean)
