package evolution.compiler.phases.typer.config

import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified

/**
 * A Native symbol (like a native function), together with its type and its compiled value
 */
case class NativeSymbol(symbol: String, tpe: Qualified[Scheme], value: Any)
