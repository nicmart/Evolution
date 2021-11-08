package evolution.compiler.tree

import TreeF.*

object PrettyPrintTree:

  private val maxChildrenLegnth = 100

  def apply(tree: Tree): String = Tree.catamorphism(prettyPrintTreeF)(tree)(0)

  private[tree] def prettyPrintTreeF(treeF: TreeF[Int => String]): Int => String =
    n =>
      indent(n) + (treeF match {
        case TreeF.App(g, args, _) => ppFunc("App", g :: args.toList, n)
        case Let(id, expr, in, _)  => ppFunc("Let", List(n => s"${"  " * n}$id", expr, in), n)
        case Lambda(id, expr, _)   => ppLambda(id, expr, n)
        case id: Id                => id.name
        case DoubleLiteral(d, _)   => d.toString
        case Bool(b, _)            => b.toString
        case IntLiteral(n, _)      => n.toString
        case Lst(ts, _)            => ppFunc("Lst", ts, n)
      })

  private def ppLambda(id: String, expr: Int => String, n: Int): String =
    val childrenLength = expr(0).length
    if childrenLength < maxChildrenLegnth then s"$id -> { ${expr(0)} }"
    else s"$id -> {\n${expr(n + 1)}\n${indent(n)}}"

  private def ppFunc(name: String, children: List[Int => String], n: Int): String =
    // val childrenInOneLine = children.map(child => child(0)).mkString(", ").toString
    // Getting the size of childrenInOneLine makes fastOpt hang
//    if (childrenInOneLine.size < maxChildrenLegnth) childrenInOneLine
//    else children.map(_.apply(n + 1)).mkString(s"$name(\n", ",\n", s"\n${indent(n)})")
    children.map(_.apply(n + 1)).mkString(s"$name(\n", ",\n", s"\n${indent(n)})")
  //children.map(child => child(0)).mkString(", ")

  private def indent(n: Int): String = "  " * n
