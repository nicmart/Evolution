package evolution.compiler.phases.parsing

import evolution.compiler.phases.parsing.ParserConfig._
import fastparse._
import evolution.compiler.tree.Tree
import PrecedenceGroup.BinaryOperator

case class PrecedenceGroup(operators: (String, BinaryOperator)*) {
  def parser[_: P](next: () => P[Tree]): P[Tree] = P(next() ~/ (opsParser ~/ next()).rep).map {
    case (head, tail) => evalAssocBinaryOp(head, tail.toList)
  }

  private def opsParser[_: P]: P[BinaryOperator] = operators.foldLeft[P[BinaryOperator]](Fail) {
    case (accParser, (opString, ast)) =>
      accParser | P(opString ~ &(allowedCharsAfterOp)).map(_ => ast)
  }

  private def evalAssocBinaryOp(head: Tree, tail: List[(BinaryOperator, Tree)]): Tree =
    tail match {
      case Nil => head
      case (op, tailHead) :: tailTail =>
        evalAssocBinaryOp(op(head, tailHead), tailTail)
    }

  private def allowedCharsAfterOp[_: P]: P[Unit] = CharIn("a-zA-Z0-9\\- \n\r")
}

object PrecedenceGroup {
  type BinaryOperator = (Tree, Tree) => Tree
}
