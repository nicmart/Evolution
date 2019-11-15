package evolution.compiler.tree

import TreeF._

object PrettyPrintTree {
  def apply(tree: Tree): String = Tree.catamorphism(prettyPrintTreeF)(tree)

  private[tree] def prettyPrintTreeF(treeF: TreeF[String]): String = treeF match {
    case TreeF.App(g, args) => ppFunc("App", g :: args.toList)
    case Let(id, expr, in)  => ppFunc("Let", List(id, expr, in))
    case Lambda(id, expr)   => ppFunc("Lambda", List(id, expr))
    case id: Id             => id.name
    case DoubleLiteral(d)   => d.toString
    case Bool(b)            => b.toString
    case IntLiteral(n)      => n.toString
    case Lst(ts)            => ppFunc("Lst", ts)
  }

  private def ppFunc(name: String, children: List[String]): String =
    children.mkString(s"$name(", ",", ")")
}
