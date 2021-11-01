package evolution.compiler.phases.typer.predicates.model

import evolution.compiler.phases.typer.model.{Assignment, Substitution}
import evolution.compiler.types.Type

private[predicates] final case class Alternatives(variable: String, alternatives: Set[Type]):
  def isFinal: Boolean = alternatives.size == 1
  def assignment: Option[Assignment] = alternatives.headOption.map(Assignment(variable, _))
  def isCompatibleWithSubstitution(subst: Substitution): Boolean =
    subst.assignments.forall(isCompatibleWithAssignment)
  def isCompatibleWithAssignment(assignment: Assignment): Boolean =
    assignment.variable != variable || alternatives.contains(assignment.tpe)
