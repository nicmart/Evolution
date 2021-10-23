package evolution.compiler.tree

import evolution.compiler.tree.AnnotatedTree.AwaitingAnnotation
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified

object TypedTreeTypes {
  type F[T] = AwaitingAnnotation[Qualified[Type]]
}

object TypedTree extends TreeBuilder[TypedTree, TypedTreeTypes.F] {
  override def toF: TreeF[TypedTree] => AwaitingAnnotation[Qualified[Type]] = AwaitingAnnotation[Qualified[Type]]
}
