package evolution.compiler.phases.parsing

import evolution.compiler.ast.AST
import evolution.compiler.phases.parsing.ParserConfig.White._
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
      case Nil                        => head
      case (op, tailHead) :: tailTail => AST.App(AST.App(op, head), evalAssocBinaryOp(tailHead, tailTail))
    }
}
