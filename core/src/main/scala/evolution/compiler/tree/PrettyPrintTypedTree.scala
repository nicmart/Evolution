package evolution.compiler.tree

import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type

object PrettyPrintTypedTree {
  def apply(tree: TypedTree): String = AnnotatedTree.catamorphism(prettyPrintTreeF)(tree)

  private def prettyPrintTreeF(tpe: Qualified[Type], treeF: TreeF[String]): String =
    s"${PrettyPrintTree.prettyPrintTreeF(treeF)}: ${tpe.value}"

}
