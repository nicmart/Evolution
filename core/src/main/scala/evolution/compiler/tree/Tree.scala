package evolution.compiler.tree

import cats.implicits._

final case class Tree(value: TreeF[Tree])

object Tree extends TreeBuilder[Tree, [T] =>> T]:
  override def toF: TreeF[Tree] => Tree = Tree.apply
  def catamorphism[A](f: TreeF[A] => A)(tree: Tree): A =
    f(tree.value.map(catamorphism(f)))
