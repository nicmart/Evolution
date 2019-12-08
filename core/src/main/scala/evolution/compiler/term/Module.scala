package evolution.compiler.term

import evolution.compiler.phases.typer.model.Assumptions

final case class Module(assumptions: Assumptions, load: Term => Term) {
  def compose(other: Module): Module =
    Module(assumptions.merge(other.assumptions), other.load andThen load)
}

object Module {
  val empty: Module = Module(Assumptions.empty, identity)
}
