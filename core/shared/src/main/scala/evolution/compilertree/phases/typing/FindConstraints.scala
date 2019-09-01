package evolution.compilertree.phases.typing

import evolution.compilertree.ast.AST
import evolution.compilertree.ast.AST._
import evolution.compilertree.types.Type
import evolution.compilertree.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compilertree.phases.typing.model.Constraints

object FindConstraints {
  def find(expr: AST): Either[String, Constraints] = {
    // TODO as you can see here predicates are extracted just for identifiers
    val nodeConstraints: Either[String, Constraints] = expr match {
      case Identifier(_, qt, _)            => Constraints.empty.withPredicates(qt.predicates).asRight
      case DoubleLiteral(_, qualifiedType) => Constraints(qualifiedType.value -> Type.Dbl).asRight
      case IntLiteral(_, qualifiedType) =>
        Constraints.empty.withPredicate(Predicate("Num", List(qualifiedType.value))).asRight
      case Bool(_, qualifiedType) => Constraints(qualifiedType.value -> Type.Bool).asRight
      case App(f, x, qualifiedType) =>
        Constraints(f.qualifiedType.value -> (x.qualifiedType.value =>: qualifiedType.value)).asRight
      case Lambda(_, _, _) => Constraints.empty.asRight
      case Let(_, _, _, _) => Constraints.empty.asRight
      case Lst(ts, qualifiedType) =>
        Type.unwrapLst(qualifiedType.value).map { inner =>
          Constraints(ts.map(t => inner -> t.qualifiedType.value): _*)
        }
    }

    val childrenConstraints = expr.children.traverse(find)

    (nodeConstraints, childrenConstraints).mapN { (n, c) =>
      n.merge(c)
    }
  }
}
