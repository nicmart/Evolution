package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type

final case class Assignment(variable: String, tpe: Type)
