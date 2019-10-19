package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate

private[typer] sealed trait Constraint

private[typer] object Constraint {
  case class Eq(a: Type, b: Type) extends Constraint {
    override def toString = s"$a = $b"
  }

  case class Pred(predicate: Predicate) extends Constraint {
    override def toString = s"${predicate.id} ${predicate.types.mkString(" ")}"
  }
}
