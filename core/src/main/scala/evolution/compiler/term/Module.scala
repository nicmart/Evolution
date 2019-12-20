package evolution.compiler.term

import Term._

import evolution.compiler.phases.typer.model.Assumptions

final case class Module(definitions: List[Definition]) {
  val assumptions: Assumptions = Assumptions(definitions.map(_.assumption))
  def compose(other: Module): Module =
    Module(definitions ++ other.definitions)

  def load(term: Term): Term =
    definitions.foldRight(term) { (definition, term) =>
      Let(definition.name, definition.term, term)
    }

  def findDefinition(name: String): Option[Definition] = definitions.find(_.name == name)
}

object Module {
  val empty: Module = Module(Nil)
}
