package evolution.compiler.term

import evolution.compiler.phases.typer.model.Assumption
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified

/**
 * Define an alias for a term.
 *
 * Basically it is a Let term without the body
 */
case class Definition(name: String, term: Option[Term], tpe: Qualified[Scheme]):
  val assumption: Assumption = Assumption(name, tpe)
