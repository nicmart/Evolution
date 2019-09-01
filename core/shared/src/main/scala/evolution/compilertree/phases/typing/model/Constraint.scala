package evolution.compilertree.phases.typing.model

import evolution.compilertree.types.Type
import evolution.compilertree.types.TypeClasses.Predicate

sealed trait Constraint
object Constraint {
  case class Eq(a: Type, b: Type) extends Constraint {
    override def toString = s"$a = $b"
  }

  case class Pred(predicate: Predicate) extends Constraint {
    override def toString = s"${predicate.id} ${predicate.types.mkString(" ")}"
  }
}