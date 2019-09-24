package evolution.compiler.tree
import cats.implicits._

final case class AnnotatedTree[A](annotation: A, tree: TreeF[AnnotatedTree[A]])

object AnnotatedTree {
  def catamorphism[A, B](f: (B, TreeF[A]) => A)(annotatedTree: AnnotatedTree[B]): A =
    f(annotatedTree.annotation, annotatedTree.tree.map(catamorphism(f)))
}
