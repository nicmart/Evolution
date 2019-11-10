package evolution.compiler.phases.typer

import evolution.compiler.types.Type
import cats.implicits._
import evolution.compiler.phases.typer.model.{Constraint, Constraints, Substitution, Unification}

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

  def mostGeneralUnifier(a: Type, b: Type): Either[String, Substitution] = (a, b) match {
    case (Type.Arrow(a1, a2), Type.Arrow(b1, b2)) =>
      for {
        s1 <- mostGeneralUnifier(a1, b1)
        s2 <- mostGeneralUnifier(s1.substitute(a2), s1.substitute(b2))
      } yield s1.andThen(s2)
    case (Type.Var(x), t)           => varBind(x, t)
    case (t, Type.Var(x))           => varBind(x, t)
    case (Type.Lst(a), Type.Lst(b)) => mostGeneralUnifier(a, b)
    case (Type.Evo(a), Type.Evo(b)) => mostGeneralUnifier(a, b)
    case _ if a == b                => Right(Substitution.empty)
    case _                          => Left(s"$a can't be unified with $b")
  }

  private def varBind(name: String, tpe: Type): Either[String, Substitution] =
    if (tpe == Type.Var(name)) Right(Substitution.empty)
    else if (tpe.typeVarUsages(name).isEmpty) Right(Substitution(name -> tpe))
    else Left(s"$name cannot be bound to $tpe")
}
