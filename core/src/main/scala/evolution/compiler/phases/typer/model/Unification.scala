package evolution.compiler.phases.typer.model

import evolution.compiler.types.TypeClasses.Predicate

private[typer] final case class Unification(substitution: Substitution, predicates: List[Predicate]) {
  def compose(s2: Substitution): Unification = copy(substitution = substitution.compose(s2))
  def withPredicate(predicate: Predicate): Unification = copy(predicates = predicate :: predicates)
  def substitutedPredicates: List[Predicate] = substitution.substitute(predicates)
}

private[typer] object Unification {
  val empty = Unification(Substitution.empty, Nil)
}
