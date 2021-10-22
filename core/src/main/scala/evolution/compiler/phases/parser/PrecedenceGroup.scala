/*
 * Copyright 2017-2021 Lenses.io Ltd
 */
package evolution.compiler.phases.parser

import cats.parse.{Parser => P}
import evolution.compiler.phases.parser.Instances._
import evolution.compiler.phases.parser.PrecedenceGroup.BinaryOperator
import evolution.compiler.tree.Tree

private[parser] final case class PrecedenceGroup(operators: (String, BinaryOperator)*) {
  def parser(next: P[Tree]): P[Tree] = (next ~~ (opsParser ~~ next).rep0).map {
    case (head, tail) => evalAssocBinaryOp(head, tail)
  }

  private def opsParser: P[BinaryOperator] = operators.foldLeft[P[BinaryOperator]](P.fail) {
    case (accParser, (opString, ast)) =>
      accParser | (P.string(opString).soft ~ allowedCharsAfterOp.peek).as(ast)
  }

  @scala.annotation.tailrec
  private def evalAssocBinaryOp(head: Tree, tail: List[(BinaryOperator, Tree)]): Tree =
    tail match {
      case Nil => head
      case (op, tailHead) :: tailTail =>
        evalAssocBinaryOp(op(head, tailHead), tailTail)
    }

  private def allowedCharsAfterOp: P[Unit] = P.charInPattern("[a-zA-Z0-9\\- \n\r()@._]").void
}

object PrecedenceGroup {
  type BinaryOperator = (Tree, Tree) => Tree
}
