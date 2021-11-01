package evolution.compiler.phases.typer.config

import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified

case class Const(name: String, tpe: Qualified[Scheme], value: Any)
