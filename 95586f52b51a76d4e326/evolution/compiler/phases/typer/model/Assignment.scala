package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type

private[typer] final case class Assignment(variable: String, tpe: Type)
