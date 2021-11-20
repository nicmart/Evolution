package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type

/**
 * Assign a type to a type variable
 */
private[typer] case class Assignment(typeVariable: String, tpe: Type)
