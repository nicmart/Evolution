package evolution.compiler.tree

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified

object PrettyPrintTypedTree:
  def apply(tree: TypedTree): String = AnnotatedTree.catamorphism(prettyPrintTreeF)(tree)(0)

  private def prettyPrintTreeF(tpe: Qualified[Type], treeF: TreeF[Int => String]): Int => String =
    n => s"${PrettyPrintTree.prettyPrintTreeF(treeF)(n)}: $tpe"

