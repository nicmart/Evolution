package evolution.compilertree.phases.parsing

import evolution.compilertree.phases.parsing.ParserConfig.White._
import fastparse.noApi._
import evolution.compilertree.ast.TreeF.Tree
import evolution.compilertree.ast.TreeF

case class PrecedenceGroup(operators: (String, Tree)*) {
  def parser(next: Parser[Tree]): Parser[Tree] = P(next ~/ (opsParser ~/ next).rep).map {
    case (head, tail) => evalAssocBinaryOp(head, tail.toList)
  }

  private def opsParser: Parser[Tree] = operators.foldLeft[Parser[Tree]](Fail) {
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
