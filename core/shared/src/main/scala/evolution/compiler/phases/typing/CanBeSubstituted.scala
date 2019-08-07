package evolution.compiler.phases.typing

import evolution.compiler.ast.AST
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.{ Predicate, Qualified }

trait CanBeSubstituted[T] {
  def substitute(s: Substitution, t: T): T
}

object CanBeSubstituted {
  implicit val `type`: CanBeSubstituted[Type] = new CanBeSubstituted[Type] {
    def substitute(s: Substitution, t: Type): Type = t match {
      case Type.Var(name)       => s.lookup(name).getOrElse(t)
      case Type.Evo(inner)      => Type.Evo(substitute(s, inner))
      case Type.Lst(inner)      => Type.Lst(substitute(s, inner))
      case Type.Arrow(from, to) => Type.Arrow(substitute(s, from), substitute(s, to))
      case _                    => t
    }
  }

  implicit val assignment: CanBeSubstituted[Assignment] = new CanBeSubstituted[Assignment] {
    def substitute(s: Substitution, a: Assignment): Assignment = a.copy(tpe = s.substitute(a.tpe))
  }

  implicit val subst: CanBeSubstituted[Substitution] = new CanBeSubstituted[Substitution] {
    def substitute(s1: Substitution, s2: Substitution): Substitution = Substitution(s1.substitute(s2.assignments))
  }

  implicit val canBeSubstituted: CanBeSubstituted[Predicate] = new CanBeSubstituted[Predicate] {
    def substitute(s: Substitution, predicate: Predicate): Predicate =
      predicate.copy(types = s.substitute(predicate.types))
  }

  implicit def list[T](implicit inner: CanBeSubstituted[T]): CanBeSubstituted[List[T]] =
    new CanBeSubstituted[List[T]] {
      def substitute(s1: Substitution, ts: List[T]): List[T] = ts.map(s1.substitute[T])
    }

  implicit def qualified[T](implicit inner: CanBeSubstituted[T]): CanBeSubstituted[Qualified[T]] =
    new CanBeSubstituted[Qualified[T]] {
      def substitute(s: Substitution, qt: Qualified[T]): Qualified[T] =
        Qualified(s.substitute(qt.predicates), s.substitute(qt.t))
    }

  implicit val ast: CanBeSubstituted[AST] = new CanBeSubstituted[AST] {
    def substitute(s: Substitution, ast: AST): AST =
      AST.transformRecursively(ast, tree => tree.withType(s.substitute(tree.tpe)))
  }

  implicit val constraint: CanBeSubstituted[Constraint] = new CanBeSubstituted[Constraint] {
    def substitute(s: Substitution, constraint: Constraint): Constraint =
      constraint match {
        case Constraint.Eq(a, b) => Constraint.Eq(s.substitute(a), s.substitute(b))
        case Constraint.Pred(p)  => Constraint.Pred(s.substitute(p))
      }
  }

  implicit val constraints: CanBeSubstituted[Constraints] = new CanBeSubstituted[Constraints] {
    def substitute(s: Substitution, constraints: Constraints): Constraints =
      Constraints(s.substitute(constraints.constraints))
  }
}
