package evolution.compiler

import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type

package object tree {
  final type TypedTree = AnnotatedTree[Qualified[Type]]
}
