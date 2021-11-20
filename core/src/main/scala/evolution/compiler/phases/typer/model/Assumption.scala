package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified

/**
 * Associate a symbol (an identifier in the user code) to a qualified scheme.
 *
 * In other words, it defines the type of the symbol.
 */
case class Assumption(symbol: String, qualifiedScheme: Qualified[Scheme]):
  def freeTypeVars: Set[String] =
    qualifiedScheme.value.freeVars ++ qualifiedScheme.predicatesTypeVars.diff(qualifiedScheme.value.vars.toSet)
