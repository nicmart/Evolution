package evolution.compiler.phases.typer
import evolution.compiler.phases.Typer
import evolution.compiler.module.Module
import evolution.compiler.tree.Tree
import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import evolution.compiler.tree._
import model.TypeVarGenerator

final class RecursiveTyper extends Typer {
  def typeTree(tree: Tree, expectedType: Option[Type], module: Module): Either[String, TypedTree] = ???

//   private case class AssignmentState(vars: TypeVarGenerator, bindings: TypeBindings,) {
//     def next: AssignmentState = copy(vars = vars.next)
//     def withBinding(name: String, tpe: Qualified[Type]): AssignmentState = copy(
//       bindings = bindings.withVarBinding(name, tpe)
//     )
//   }

  //private type S[T] =
}
