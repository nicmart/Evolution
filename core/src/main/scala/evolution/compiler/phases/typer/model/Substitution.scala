package evolution.compiler.phases.typer.model

import cats.implicits._
import evolution.compiler.types.Type

private[phases] final case class Substitution(assignments: List[Assignment]) {
  def without(variable: String): Substitution = Substitution(assignments.filterNot(_.variable == variable))
  def lookup(variable: String): Option[Type] = assignments.find(_.variable == variable).map(_.tpe)
  def substitute[T](t: T)(implicit cbs: CanBeSubstituted[T]): T = cbs.substitute(this, t)
  def compose(s2: Substitution): Substitution = s2.andThen(this)
  def andThen(s2: Substitution): Substitution = Substitution(s2.substitute(this).assignments ++ s2.assignments)
  def mergeOpt(s2: Substitution): Option[Substitution] = merge(s2).toOption
  def merge(s2: Substitution): Either[String, Substitution] = {
    val commonVars = assignments.map(_.variable).intersect(s2.assignments.map(_.variable))
    val agree = commonVars.forall { variable =>
      substitute[Type](Type.Var(variable)) == s2.substitute[Type](Type.Var(variable))
    }
    if (agree) Substitution(assignments ++ s2.assignments).asRight
    else "Merge has failed".asLeft
  }
}

private[typer] object Substitution {
  def apply(assignments: (String, Type)*): Substitution =
    Substitution(assignments.toList.map {
      case (s, t) => Assignment(s, t)
    })
  val empty: Substitution = Substitution(Nil)
}
