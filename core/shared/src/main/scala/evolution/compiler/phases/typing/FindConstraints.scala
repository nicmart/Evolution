package evolution.compiler.phases.typing

import evolution.compiler.ast.AST
import evolution.compiler.ast.AST.{ App, Bool, DoubleLiteral, Identifier, IntLiteral, Lambda, Let }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compiler.phases.typing.model.Constraints

object FindConstraints {
  def find(expr: AST): Either[String, Constraints] = {
    val nodeConstraints = expr match {
      case Identifier(_, qt, _)  => Constraints.empty.withPredicates(qt.predicates).asRight[String]
      case DoubleLiteral(_, tpe) => Constraints(tpe.t -> Type.Dbl).asRight[String]
      case IntLiteral(_, tpe)    => Constraints.empty.withPredicate(Predicate("Num", List(tpe.t))).asRight[String]
      case Bool(_, tpe)          => Constraints(tpe.t -> Type.Bool).asRight[String]
      case App(f, x, tpe)        => Constraints(f.tpe.t -> (x.tpe.t =>: tpe.t)).asRight[String]
      case Lambda(_, _, _) =>
        Constraints.empty.asRight[String]
      case Let(_, _, _, _) =>
        Constraints.empty.asRight[String]
    }

    val childrenConstraints = expr.children.traverse(find)

    (nodeConstraints, childrenConstraints).mapN { (n, c) =>
      n.merge(c)
    }
  }
}
