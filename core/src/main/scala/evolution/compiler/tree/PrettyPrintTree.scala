package evolution.compiler.tree

import TreeF._

object PrettyPrintTree {
  def apply(tree: Tree): String = Tree.catamorphism(prettyPrintTreeF)(tree)(0)

  private[tree] def prettyPrintTreeF(treeF: TreeF[Int => String]): Int => String = n => {
    val indent = "  " * n
    indent + (treeF match {
      case TreeF.App(g, args) => ppFunc("App", g :: args.toList, n)
      case Let(id, expr, in)  => ppFunc("Let", List(n => s"${"  " * n}$id", expr, in), n)
      case Lambda(id, expr)   => s"$id -> " + ppFunc("", List(expr), n, "{", "}")
      case id: Id             => id.name
      case DoubleLiteral(d)   => d.toString
      case Bool(b)            => b.toString
      case IntLiteral(n)      => n.toString
      case Lst(ts)            => ppFunc("Lst", ts, n)
    })
  }

  private def ppFunc(
      name: String,
      children: List[Int => String],
      n: Int,
      open: String = "(",
      close: String = ")",
      newLine: String = "\n"
  ): String = {
    val indent = "  " * n
    children.map(child => child(n + 1)).mkString(s"$name$open$newLine", s",$newLine", s"$newLine$indent$close")
  }
}
