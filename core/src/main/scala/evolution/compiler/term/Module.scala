package evolution.compiler.term

import Term._

import evolution.compiler.phases.typer.model.Assumptions

case class Module(definitions: List[Definition]):
  val assumptions: Assumptions = Assumptions(definitions.map(_.assumption))

  val terms: Map[String, Term] = Map.from(definitions.flatMap(d => d.term.map(d.name -> _)))

  def compose(other: Module): Module =
    Module(definitions ++ other.definitions)

  def load(term: Term): Term =
    definitions.foldRight(term) { (definition, term) =>
      definition.term.fold(term)(Let(definition.name, _, term))
    }

  def findDefinition(name: String): Option[Definition] = definitions.find(_.name == name)

object Module:
  val empty: Module = Module(Nil)
