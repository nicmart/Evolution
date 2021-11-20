package evolution.compiler.phases.typer.predicates.model

import cats.implicits.*
import evolution.compiler.phases.typer.model.{Assignment, Substitution}
import evolution.compiler.types.TypeClasses.Predicate

private[predicates] case class PredicateConditions(predicate: Predicate, substitutions: Set[Substitution]):
  lazy val requirements: Map[String, Alternatives] =
    substitutions.flatMap(_.assignments).groupBy(_.typeVariable).map { case (variable, assignments) =>
      variable -> Alternatives(variable, assignments.map(_.tpe))
    }
  lazy val isFinal: Boolean = requirements.values.forall(_.isFinal)
  def nonEmpty: Boolean = substitutions.nonEmpty
  def assignments: List[Assignment] = requirements.values.flatMap(_.assignment).toList
  def substitution: Either[String, Substitution] =
    Either.cond(nonEmpty, Substitution(assignments), s"Predicate $predicate has no possible substitutions")

  def reduce(otherRequirements: Map[String, Alternatives]): Either[String, PredicateConditions] =
    PredicateConditions(
      predicate,
      substitutions.filter(subst => otherRequirements.values.forall(_.isCompatibleWithSubstitution(subst)))
    ).asRight.filterOrElse(_.nonEmpty, s"Predicate $predicate is incompatible with requirements $otherRequirements")
