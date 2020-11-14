package evolution.compiler.tree

import evolution.compiler.tree.AnnotatedTree.AwaitingAnnotation
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified

object TypedTree extends TreeBuilder[TypedTree, λ[T => AwaitingAnnotation[Qualified[Type]]]] {
  override def toF: TreeF[TypedTree] => AwaitingAnnotation[Qualified[Type]] = AwaitingAnnotation[Qualified[Type]]
}
