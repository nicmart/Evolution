package evolution.compilertree.phases.typing.model

import evolution.compilertree.types.Type

final case class Assignment(variable: String, tpe: Type)
