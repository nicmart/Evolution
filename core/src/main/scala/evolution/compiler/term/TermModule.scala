package evolution.compiler.term

import evolution.compiler.phases.typer.model.Assumptions

final case class TermModule(assumptions: Assumptions, load: Term => Term) {
  def compose(other: TermModule): TermModule =
    TermModule(assumptions.merge(other.assumptions), other.load andThen load)
}

object TermModule {
  val empty: TermModule = TermModule(Assumptions.empty, identity)
}
