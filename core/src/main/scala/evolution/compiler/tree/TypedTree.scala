package evolution.compiler.tree

import evolution.compiler.tree.AnnotatedTree.WaitingAnnotation
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified

object TypedTree extends TreeBuilder[TypedTree, λ[T => WaitingAnnotation[Qualified[Type]]]] {
  override def toF: TreeF[TypedTree] => WaitingAnnotation[Qualified[Type]] = WaitingAnnotation[Qualified[Type]]
}
