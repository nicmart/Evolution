package evolution.compiler.phases.typer.model

final class Assumptions(private val assumptions: Map[String, Assumption]) {

  def all: Map[String, Assumption] = assumptions

  def allFreeTypeVars: Set[String] = assumptions.values.flatMap(_.freeTypeVars).toSet

  def vars: List[String] = assumptions.map(_._1).toList

  def get(name: String): Option[Assumption] = assumptions.get(name)

  def merge(other: Assumptions): Assumptions =
    new Assumptions(assumptions ++ other.assumptions)

  def withAssumption(assumption: Assumption): Assumptions =
    new Assumptions(assumptions.updated(assumption.name, assumption))
}

object Assumptions {
  val empty: Assumptions = new Assumptions(Map.empty)
}
