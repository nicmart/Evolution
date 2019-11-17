package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified

final case class Assumption(name: String, qualifiedScheme: Qualified[Scheme], primitive: Boolean)
