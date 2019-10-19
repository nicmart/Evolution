package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate

private[typer] final case class Constraints(constraints: List[Constraint]) {
  def merge(other: Constraints): Constraints = Constraints(constraints ++ other.constraints)
  def merge(other: List[Constraints]): Constraints = other.foldLeft(this) { (constraints, current) =>
    constraints.merge(current)
  }
  def withPredicate(predicate: Predicate): Constraints =
    Constraints(Constraint.Pred(predicate) :: constraints)

  def withPredicates(predicates: List[Predicate]): Constraints =
    Constraints(predicates.map(Constraint.Pred) ++ constraints)

  override def toString: String = constraints.mkString("\n")
}

private[typer] object Constraints {
  val empty: Constraints = Constraints(Nil)
  def apply(constraints: (Type, Type)*): Constraints = Constraints(constraints.toList.map {
    case (a, b) => Constraint.Eq(a, b)
  })
}
