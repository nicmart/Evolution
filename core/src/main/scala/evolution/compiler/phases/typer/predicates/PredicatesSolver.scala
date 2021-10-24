package evolution.compiler.phases.typer.predicates

import cats.implicits._
import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.phases.typer.predicates.model.PredicateConditions

private[predicates] final case class PredicatesSolver(
    current: PredicateConditions,
    otherConditions: Vector[PredicateConditions]
) {
  def total: Int = 1 + otherConditions.size
  def allConditions: Seq[PredicateConditions] = current +: otherConditions
  lazy val isFinal: Boolean = current.isFinal && otherConditions.forall(_.isFinal)

  def substitution: Either[String, Substitution] =
    allConditions.toList.traverse(_.substitution).map { ss =>
      ss.fold(Substitution.empty)((s1, s2) => s1.compose(s2))
    }

  def next: Either[String, PredicatesSolver] = {
    val requirements = current.requirements
    for
      otherConditionsReduced <- otherConditions.traverse(_.reduce(requirements))
      conditions = otherConditionsReduced :+ current
      nextSolver = PredicatesSolver(conditions.head, conditions.tail)
    yield nextSolver
  }

  def nextN(n: Int): Either[String, PredicatesSolver] =
    if n <= 0 then this.asRight
    else next.flatMap(_.nextN(n - 1))

  def cycle: Either[String, PredicatesSolver] = nextN(total)

  def solve: Either[String, Substitution] = {
    val reducedN = cycle
    reducedN match {
      case Left(value)                             => Left(value)
      case Right(nextSolver) if nextSolver == this => this.substitution
      case Right(nextSolver)                       => nextSolver.solve
    }
  }
}
