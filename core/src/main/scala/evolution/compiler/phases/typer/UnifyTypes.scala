package evolution.compiler.phases.typer

import evolution.compiler.types.Type
import cats.implicits._
import evolution.compiler.phases.typer.model.{ Constraint, Constraints, Substitution, Unification }

private[typer] object UnifyTypes {
  def unify(constraints: Constraints): Either[String, Unification] =
    constraints.constraints match {
      case Nil => Unification.empty.asRight
      case head :: tail =>
        head match {
          // TODO ifs prevents exhaustive checking...
          case Constraint.Eq(a, b) if a == b => unify(Constraints(tail))
          case Constraint.Eq(Type.Var(x), t) if t.typeVarUsages(x).isEmpty =>
            val substituteVar = Substitution(x -> t)
            val constraints2 = substituteVar.substitute(Constraints(tail))
            unify(constraints2).map(_.compose(substituteVar))
          case Constraint.Eq(t, Type.Var(x)) if t.typeVarUsages(x).isEmpty =>
            val substituteVar = Substitution(x -> t)
            val constraints2 = substituteVar.substitute(Constraints(tail))
            unify(constraints2).map(_.compose(substituteVar))
          case Constraint.Eq(Type.Evo(a), Type.Evo(b)) =>
            unify(Constraints(a -> b).merge(Constraints(tail)))
          case Constraint.Eq(Type.Lst(a), Type.Lst(b)) =>
            unify(Constraints(a -> b).merge(Constraints(tail)))
          case Constraint.Eq(Type.Arrow(a1, b1), Type.Arrow(a2, b2)) =>
            unify(Constraints(a1 -> a2, b1 -> b2).merge(Constraints(tail)))
          case Constraint.Pred(p) => unify(Constraints(tail)).map(_.withPredicate(p))
          case _                  => s"$head constraint can't be unified".asLeft
        }
    }
}
