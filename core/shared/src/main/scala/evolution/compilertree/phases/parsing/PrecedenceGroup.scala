package evolution.compilertree.phases.parsing

import evolution.compilertree.ast.AST
import evolution.compilertree.phases.parsing.ParserConfig.White._
import fastparse.noApi._

case class PrecedenceGroup(operators: (String, AST)*) {
  def parser(next: Parser[AST]): Parser[AST] = P(next ~/ (opsParser ~/ next).rep).map {
    case (head, tail) => evalAssocBinaryOp(head, tail.toList)
  }

  private def opsParser: Parser[AST] = operators.foldLeft[Parser[AST]](Fail) {
    case (accParser, (opString, ast)) =>
      accParser | P(opString).map(_ => ast)
  }

  private def evalAssocBinaryOp(head: AST, tail: List[(AST, AST)]): AST =
    tail match {
      case Nil => head
      case (op, tailHead) :: tailTail =>
        evalAssocBinaryOp(binary(head, op, tailHead), tailTail)
    }

  private def binary(left: AST, op: AST, right: AST): AST =
    AST.App(AST.App(op, left), right)
}
