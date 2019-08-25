package evolution.compiler.phases.typing

import evolution.compiler.ast.AST
import evolution.compiler.ast.AST._
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compiler.phases.typing.model.Constraints

object FindConstraints {
  def find(expr: AST): Either[String, Constraints] = {
    // TODO as you can see here predicates are extracted just for identifiers
    val nodeConstraints: Either[String, Constraints] = expr match {
      case Identifier(_, qt, _)  => Constraints.empty.withPredicates(qt.predicates).asRight
      case DoubleLiteral(_, tpe) => Constraints(tpe.t -> Type.Dbl).asRight
      case IntLiteral(_, tpe)    => Constraints.empty.withPredicate(Predicate("Num", List(tpe.t))).asRight
      case Bool(_, tpe)          => Constraints(tpe.t -> Type.Bool).asRight
      case App(f, x, tpe)        => Constraints(f.tpe.t -> (x.tpe.t =>: tpe.t)).asRight
      case Lambda(_, _, _)       => Constraints.empty.asRight
      case Let(_, _, _, _)       => Constraints.empty.asRight
      case Lst(ts, tpe) =>
        Type.unwrapLst(tpe.t).map { inner =>
          Constraints(ts.map(t => inner -> t.tpe.t): _*)
        }
    }

    val childrenConstraints = expr.children.traverse(find)

    (nodeConstraints, childrenConstraints).mapN { (n, c) =>
      n.merge(c)
    }
  }
}
