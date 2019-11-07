package evolution.compiler.types

final class Assumptions(private val assumptions: Map[String, Assumption]) {

  def all = assumptions

  def get(name: String): Option[Assumption] = assumptions.get(name)

  def merge(other: Assumptions): Assumptions =
    new Assumptions(assumptions ++ other.assumptions)

  def withAssumption(assumption: Assumption): Assumptions =
    new Assumptions(assumptions.updated(assumption.name, assumption))
}

object Assumptions {
  val empty: Assumptions = new Assumptions(Map.empty)
}
