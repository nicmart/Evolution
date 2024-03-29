package evolution.compiler.tree
import cats.implicits.*

final case class AnnotatedTree[+A](annotation: A, tree: TreeF[AnnotatedTree[A]])

object AnnotatedTree:
  def catamorphism[A, B](f: (B, TreeF[A]) => A)(annotatedTree: AnnotatedTree[B]): A =
    f(annotatedTree.annotation, annotatedTree.tree.map(catamorphism(f)))

  case class AwaitingAnnotation[A](tree: TreeF[AnnotatedTree[A]]):
    def annotate(a: A): AnnotatedTree[A] = AnnotatedTree(a, tree)
    def as(a: A): AnnotatedTree[A] = annotate(a)
