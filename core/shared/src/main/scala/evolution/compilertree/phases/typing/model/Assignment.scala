package evolution.compilertree.phases.typing.model

import evolution.compiler.types.Type

final case class Assignment(variable: String, tpe: Type)
