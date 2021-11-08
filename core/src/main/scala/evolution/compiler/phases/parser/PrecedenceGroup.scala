/*
 * Copyright 2017-2021 Lenses.io Ltd
 */
package evolution.compiler.phases.parser

import cats.parse.{Parser => P}
import evolution.compiler.phases.parser.Instances.*
import evolution.compiler.phases.parser.PrecedenceGroup.BinaryOperator
import evolution.compiler.tree.Tree
import evolution.compiler.tree.Pos

private[parser] final case class PrecedenceGroup(operators: (String, BinaryOperator)*):
  def parser(next: P[Tree]): P[Tree] = (next ~~ (opsParser ~~ next).rep0).map { case (head, tail) =>
    evalAssocBinaryOp(head, tail)
  }

  private def opsParser: P[(Tree, Tree) => Tree] = operators.foldLeft[P[(Tree, Tree) => Tree]](P.fail) {
    case (accParser, (opString, ast)) =>
      accParser | (P.string(opString).onlyPos.soft <* allowedCharsAfterOp.peek).map(pos => ast(_, pos, _))
  }

  @scala.annotation.tailrec
  private def evalAssocBinaryOp(head: Tree, tail: List[((Tree, Tree) => Tree, Tree)]): Tree =
    tail match
      case Nil => head
      case (op, tailHead) :: tailTail =>
        evalAssocBinaryOp(op(head, tailHead), tailTail)

  private def allowedCharsAfterOp: P[Unit] = P.charInPattern("[a-zA-Z0-9\\- \n\r()@._]").void

object PrecedenceGroup:
  type BinaryOperator = (Tree, Pos, Tree) => Tree
