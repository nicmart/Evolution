package evolution.compiler.term

import evolution.compiler.phases.typer.model.Assumption
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified

case class Definition(name: String, term: Option[Term], tpe: Qualified[Scheme]) {
  val assumption: Assumption = Assumption(name, tpe)
}
