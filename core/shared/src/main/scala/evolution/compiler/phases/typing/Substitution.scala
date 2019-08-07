package evolution.compiler.phases.typing

import cats.Applicative
import cats.implicits._
import cats.mtl.FunctorRaise
import cats.mtl.implicits._
import evolution.compiler.types.Type
import evolution.language.Typer.CanBeSubstituted

final case class Substitution(assignments: List[Assignment]) {
  def lookup(variable: String): Option[Type] = assignments.find(_.variable == variable).map(_.tpe)
  def substitute[T](t: T)(implicit cbs: CanBeSubstituted[T]): T = cbs.substitute(this, t)
  def compose(s2: Substitution): Substitution = Substitution(substitute(s2).assignments ++ assignments)
  def mergeOpt(s2: Substitution): Option[Substitution] = merge[Either[String, ?]](s2).toOption
  def merge[M[_]](s2: Substitution)(implicit M: FunctorRaise[M, String], A: Applicative[M]): M[Substitution] = {
    val commonVars = assignments.map(_.variable).intersect(s2.assignments.map(_.variable))
    val agree = commonVars.forall { variable =>
      substitute[Type](Type.Var(variable)) == s2.substitute[Type](Type.Var(variable))
    }
    if (agree) Substitution(assignments ++ s2.assignments).pure[M]
    else M.raise("Merge has failed")
  }
}

object Substitution {
  def apply(assignments: (String, Type)*): Substitution = Substitution(assignments.toList.map {
    case (s, t) => Assignment(s, t)
  })
  val empty: Substitution = Substitution(Nil)
}
