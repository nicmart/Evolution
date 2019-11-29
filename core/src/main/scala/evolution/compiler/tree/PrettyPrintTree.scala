package evolution.compiler.tree

import TreeF._

object PrettyPrintTree {
  private val maxChildrenLegnth = 100
  def apply(tree: Tree): String = Tree.catamorphism(prettyPrintTreeF)(tree)(0)

  private[tree] def prettyPrintTreeF(treeF: TreeF[Int => String]): Int => String = n => {
    val indent = "  " * n
    indent + (treeF match {
      case TreeF.App(g, args) => ppFunc("App", g :: args.toList, n)
      case Let(id, expr, in)  => ppFunc("Let", List(n => s"${"  " * n}$id", expr, in), n)
      case Lambda(id, expr)   => ppLambda(id, expr, n)
      case id: Id             => id.name
      case DoubleLiteral(d)   => d.toString
      case Bool(b)            => b.toString
      case IntLiteral(n)      => n.toString
      case Lst(ts)            => ppFunc("Lst", ts, n)
    })
  }

  private def ppLambda(id: String, expr: Int => String, n: Int): String =
    ppFunc(s"$id -> ", List(expr), n, "{", "}")

  private def ppFunc(
      name: String,
      children: List[Int => String],
      n: Int,
      open: String = "(",
      close: String = ")"
  ): String = {
    val indent = "  " * n
    val childrenLength = children.map(_.apply(0)).mkString(", ").length
    if (childrenLength < maxChildrenLegnth) children.map(_.apply(0)).mkString(s"$name$open", ", ", close)
    else children.map(_.apply(n + 1)).mkString(s"$name$open\n", ",\n", s"\n$indent$close")
  }
}
