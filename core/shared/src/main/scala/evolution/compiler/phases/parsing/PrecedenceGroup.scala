package evolution.compiler.phases.parsing

import evolution.compiler.phases.parsing.ParserConfig._
import fastparse._
import evolution.compiler.tree.{Tree, TreeF}

case class PrecedenceGroup(operators: (String, Tree)*) {
  def parser[_: P](next: () => P[Tree]): P[Tree] = P(next() ~/ (opsParser ~/ next()).rep).map {
    case (head, tail) => evalAssocBinaryOp(head, tail.toList)
  }

  private def opsParser[_: P]: P[Tree] = operators.foldLeft[P[Tree]](Fail) {
    case (accParser, (opString, ast)) =>
      accParser | P(opString).map(_ => ast)
  }

  private def evalAssocBinaryOp(head: Tree, tail: List[(Tree, Tree)]): Tree =
    tail match {
      case Nil => head
      case (op, tailHead) :: tailTail =>
        evalAssocBinaryOp(binary(head, op, tailHead), tailTail)
    }

  private def binary(left: Tree, op: Tree, right: Tree): Tree =
    TreeF.AppN(op, left, right)
}
