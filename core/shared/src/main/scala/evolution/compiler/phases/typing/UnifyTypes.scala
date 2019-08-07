package evolution.compiler.phases.typing

import cats.Applicative
import cats.mtl.FunctorRaise
import evolution.compiler.types.Type
import cats.implicits._
import cats.mtl.implicits._

object UnifyTypes {
  def unify[M[_]](constraints: Constraints)(implicit R: FunctorRaise[M, String], A: Applicative[M]): M[Unification] =
    constraints.constraints match {
      case Nil => Unification.empty.pure[M]
      case head :: tail =>
        head match {
          case Constraint.Eq(a, b) if a == b => unify[M](Constraints(tail))
          case Constraint.Eq(Type.Var(x), t) if t.typeVarUsages(x).isEmpty =>
            val substituteVar = Substitution(x -> t)
            val constraints2 = substituteVar.substitute(Constraints(tail))
            unify[M](constraints2).map(_.compose(substituteVar))
          case Constraint.Eq(t, Type.Var(x)) if t.typeVarUsages(x).isEmpty =>
            val substituteVar = Substitution(x -> t)
            val constraints2 = substituteVar.substitute(Constraints(tail))
            unify[M](constraints2).map(_.compose(substituteVar))
          case Constraint.Eq(Type.Evo(a), Type.Evo(b)) =>
            unify[M](Constraints(a -> b).merge(Constraints(tail)))
          case Constraint.Eq(Type.Arrow(a1, b1), Type.Arrow(a2, b2)) =>
            unify[M](Constraints(a1 -> a2, b1 -> b2).merge(Constraints(tail)))
          case Constraint.Pred(p) => unify[M](Constraints(tail)).map(_.withPredicate(p))
          case _                  => s"$head constraint can't be unified".raise[M, Unification]
        }
    }

}
