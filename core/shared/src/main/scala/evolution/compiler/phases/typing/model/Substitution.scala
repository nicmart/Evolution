package evolution.compiler.phases.typing.model

import cats.implicits._
import evolution.compiler.phases.typing.model
import evolution.compiler.types.Type

final case class Substitution(assignments: List[Assignment]) {
  def lookup(variable: String): Option[Type] = assignments.find(_.variable == variable).map(_.tpe)
  def substitute[T](t: T)(implicit cbs: CanBeSubstituted[T]): T = cbs.substitute(this, t)
  def compose(s2: Substitution): Substitution = Substitution(substitute(s2).assignments ++ assignments)
  def mergeOpt(s2: Substitution): Option[Substitution] = merge(s2).toOption
  def merge(s2: Substitution): Either[String, Substitution] = {
    val commonVars = assignments.map(_.variable).intersect(s2.assignments.map(_.variable))
    val agree = commonVars.forall { variable =>
      substitute[Type](Type.Var(variable)) == s2.substitute[Type](Type.Var(variable))
    }
    if (agree) Substitution(assignments ++ s2.assignments).asRight[String]
    else "Merge has failed".asLeft[Substitution]
  }
}

object Substitution {
  def apply(assignments: (String, Type)*): Substitution = Substitution(assignments.toList.map {
    case (s, t) => model.Assignment(s, t)
  })
  val empty: Substitution = Substitution(Nil)
}
