package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.{ Predicate, Qualified }
import evolution.compiler.tree._

private[typer] trait CanBeSubstituted[T] {
  def substitute(s: Substitution, t: T): T
}

private[typer] object CanBeSubstituted {
  implicit val `type`: CanBeSubstituted[Type] = new CanBeSubstituted[Type] {
    def substitute(s: Substitution, t: Type): Type =
      Type.replaceVars(t, s.assignments.map(a => a.variable -> a.tpe))
  }

  implicit val assignment: CanBeSubstituted[Assignment] = new CanBeSubstituted[Assignment] {
    def substitute(s: Substitution, a: Assignment): Assignment = a.copy(tpe = s.substitute[Type](a.tpe))
  }

  implicit val subst: CanBeSubstituted[Substitution] = new CanBeSubstituted[Substitution] {
    def substitute(s1: Substitution, s2: Substitution): Substitution = Substitution(s1.substitute(s2.assignments))
  }

  implicit val canBeSubstituted: CanBeSubstituted[Predicate] = new CanBeSubstituted[Predicate] {
    def substitute(s: Substitution, predicate: Predicate): Predicate =
      predicate.copy(types = s.substitute(predicate.types))
  }

  implicit val tree: CanBeSubstituted[TypedTree] = new CanBeSubstituted[TypedTree] {
    def substitute(s: Substitution, typedTree: TypedTree): TypedTree =
      AnnotatedTree.catamorphism[TypedTree, Qualified[Type]]((qt, treeF) => treeF.annotate(s.substitute(qt)))(typedTree)

  }

  implicit def list[T](implicit inner: CanBeSubstituted[T]): CanBeSubstituted[List[T]] =
    new CanBeSubstituted[List[T]] {
      def substitute(s1: Substitution, ts: List[T]): List[T] = ts.map(s1.substitute[T])
    }

  implicit def qualified[T](implicit inner: CanBeSubstituted[T]): CanBeSubstituted[Qualified[T]] =
    new CanBeSubstituted[Qualified[T]] {
      def substitute(s: Substitution, qt: Qualified[T]): Qualified[T] =
        Qualified(s.substitute(qt.predicates), s.substitute(qt.value))
    }

  implicit val constraint: CanBeSubstituted[Constraint] = new CanBeSubstituted[Constraint] {
    def substitute(s: Substitution, constraint: Constraint): Constraint =
      constraint match {
        case Constraint.Eq(a, b) => Constraint.Eq(s.substitute[Type](a), s.substitute[Type](b))
        case Constraint.Pred(p)  => Constraint.Pred(s.substitute(p))
      }
  }

  implicit val constraints: CanBeSubstituted[Constraints] = new CanBeSubstituted[Constraints] {
    def substitute(s: Substitution, constraints: Constraints): Constraints =
      Constraints(s.substitute(constraints.constraints))
  }
}
