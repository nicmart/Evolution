package evolution.compiler.phases.typer.predicates.model

import evolution.compiler.phases.typer.model.{Assignment, Substitution}
import evolution.compiler.types.Type

/**
 * Possible alternatives for a given type variable
 */
private[predicates] case class Alternatives(typeVar: String, alternatives: Set[Type]):
  def isFinal: Boolean = alternatives.size == 1
  def assignment: Option[Assignment] = alternatives.headOption.map(Assignment(typeVar, _))
  def isCompatibleWithSubstitution(subst: Substitution): Boolean =
    subst.assignments.forall(isCompatibleWithAssignment)
  def isCompatibleWithAssignment(assignment: Assignment): Boolean =
    assignment.typeVariable != typeVar || alternatives.contains(assignment.tpe)
