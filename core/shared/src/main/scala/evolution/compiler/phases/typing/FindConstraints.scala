package evolution.compiler.phases.typing

import evolution.compiler.ast.AST
import evolution.compiler.ast.AST.{ App, Bool, DoubleLiteral, Identifier, IntLiteral, Lambda, Let }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import TypeInference._

object FindConstraints {
  def find[M[_]](expr: AST)(implicit TI: TypeInference[M]): M[Constraints] = {
    val nodeConstraints: M[Constraints] = expr match {
      case Identifier(_, qt, _)  => Constraints.empty.withPredicates(qt.predicates).pure[M]
      case DoubleLiteral(_, tpe) => Constraints(tpe.t -> Type.Dbl).pure[M]
      case IntLiteral(_, tpe)    => Constraints.empty.withPredicate(Predicate("Num", List(tpe.t))).pure[M]
      case Bool(_, tpe)          => Constraints(tpe.t -> Type.Bool).pure[M]
      case App(f, x, tpe)        => Constraints(f.tpe.t -> (x.tpe.t =>: tpe.t)).pure[M]
      case Lambda(_, _, _) =>
        Constraints.empty.pure[M]
      case Let(_, _, _, _) =>
        Constraints.empty.pure[M]
    }

    val childrenConstraints = expr.children.traverse(find[M])

    (nodeConstraints, childrenConstraints).mapN { (n, c) =>
      n.merge(c)
    }
  }
}
