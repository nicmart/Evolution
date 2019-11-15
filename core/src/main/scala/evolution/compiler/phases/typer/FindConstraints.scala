package evolution.compiler.phases.typer

import evolution.compiler.types.Type
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compiler.phases.typer.model.Constraints
import evolution.compiler.tree._
import evolution.compiler.tree.TreeF._

private[typer] object FindConstraints {
  def find(typedTree: TypedTree): Either[String, Constraints] = {
    // TODO as you can see here predicates are extracted just for identifiers and literals
    val exprType = typedTree.annotation
    val nodeConstraints: Either[String, Constraints] = typedTree.tree match {
      case Id(_, _)         => Constraints.empty.withPredicates(exprType.predicates).asRight
      case DoubleLiteral(_) => Constraints(exprType.value -> Type.Double).asRight
      case IntLiteral(_) =>
        Constraints.empty.withPredicate(Predicate("Num", List(exprType.value))).asRight
      case Bool(_) => Constraints(exprType.value -> Type.Bool).asRight
      case App(f, args) =>
        Constraints(f.annotation.value -> lambdaType(args.map(_.annotation.value).toList, exprType.value)).asRight
      case Lambda(_, _) => Constraints.empty.asRight
      case Let(_, _, _) => Constraints.empty.asRight
      case Lst(ts) =>
        exprType.value.unwrapLst.map { inner =>
          Constraints(ts.map(t => inner -> t.annotation.value): _*)
        }
    }

    val childrenConstraints: Either[String, List[Constraints]] = typedTree.tree.children.traverse(find)

    (nodeConstraints, childrenConstraints).mapN { (n, c) =>
      n.merge(c)
    }
  }

  private def lambdaType(inputTypes: List[Type], returnType: Type): Type = inputTypes match {
    case Nil          => returnType
    case head :: tail => head =>: lambdaType(tail, returnType)
  }
}
