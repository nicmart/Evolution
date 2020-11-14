package evolution.compiler.phases.typer

import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.types.Type

private[typer] object Unifier {
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
