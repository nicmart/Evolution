package evolution.compiler.phases.parsing

import evolution.compiler.ast.AST
import fastparse.noApi.Parser

case class PrecedenceGroups(last: Parser[AST], groups: List[PrecedenceGroup]) {
  def operand: Parser[AST] = groups.foldRight(last) { (group, accParser) =>
    group.parser(accParser)
  }

  def allOperators: List[(String, AST)] = groups.flatMap(group => group.operators)
}
