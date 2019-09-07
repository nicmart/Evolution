package evolution.compiler.phases.typing

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compiler.phases.typing.model.Constraints
import evolution.compiler.ast.TreeF.TypedTree
import evolution.compiler.ast.TreeF._

object FindConstraints {
  def find(expr: TypedTree): Either[String, Constraints] = {
    // TODO as you can see here predicates are extracted just for identifiers
    val exprType = expr.value
    val nodeConstraints: Either[String, Constraints] = expr.tail match {
      case Identifier(_, _) => Constraints.empty.withPredicates(exprType.predicates).asRight
      case DoubleLiteral(_) => Constraints(exprType.value -> Type.Dbl).asRight
      case IntLiteral(_) =>
        Constraints.empty.withPredicate(Predicate("Num", List(exprType.value))).asRight
      case Bool(_) => Constraints(exprType.value -> Type.Bool).asRight
      case App(f, args) =>
        Constraints(f.value.value -> lambdaType(args.map(_.value.value).toList, exprType.value)).asRight
      case Lambda(_, _) => Constraints.empty.asRight
      case Let(_, _, _) => Constraints.empty.asRight
      case Lst(ts) =>
        Type.unwrapLst(exprType.value).map { inner =>
          Constraints(ts.map(t => inner -> t.value.value): _*)
        }
    }

    val childrenConstraints: Either[String, List[Constraints]] = expr.tail.children.traverse(find)

    (nodeConstraints, childrenConstraints).mapN { (n, c) =>
      n.merge(c)
    }
  }

  private def lambdaType(inputTypes: List[Type], returnType: Type): Type = inputTypes match {
    case Nil          => returnType
    case head :: tail => head =>: lambdaType(tail, returnType)
  }
}
