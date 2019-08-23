package evolution.compiler.phases.parsing

import evolution.compiler.ast.AST
import evolution.compiler.phases.parsing.ParserConfig.White._
import fastparse.noApi._

case class PrecedenceGroup(operators: (String, AST)*) {
  def parser(next: Parser[AST]): Parser[AST] = P(next ~/ (opsParser ~/ next).rep).map {
    case (head, tail) =>
      val (invHead, invTail) = invert(head, tail.toList)
      evalAssocBinaryOp(invHead, invTail)
  }

  private def opsParser: Parser[AST] = operators.foldLeft[Parser[AST]](Fail) {
    case (accParser, (opString, ast)) =>
      accParser | P(opString).map(_ => ast)
  }

  private def invert(head: AST, tail: List[(AST, AST)]): (AST, List[(AST, AST)]) =
    tail match {
      case Nil => (head, Nil)
      case (op, head2) :: tail2 =>
        val (recHead, recTail) = invert(head2, tail2)
        (recHead, recTail :+ (op, head))
    }

  private def evalAssocBinaryOp(head: AST, tail: List[(AST, AST)]): AST =
    tail match {
      case Nil                        => head
      case (op, tailHead) :: tailTail => AST.App(AST.App(op, evalAssocBinaryOp(tailHead, tailTail)), head)
    }
}
