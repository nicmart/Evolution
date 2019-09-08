package evolution.compiler.tree

import cats.implicits._

final case class Tree(value: TreeF[Tree])

object Tree {
  def catamorphism[A](f: TreeF[A] => A)(tree: Tree): A =
    f(tree.value.map(catamorphism(f)))
}
