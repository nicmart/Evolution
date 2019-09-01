package evolution.compilertree.phases.parsing

import fastparse.noApi.Parser
import evolution.compilertree.ast.TreeF.Tree

case class PrecedenceGroups(last: Parser[Tree], groups: List[PrecedenceGroup]) {
  def operand: Parser[Tree] = groups.foldRight(last) { (group, accParser) =>
    group.parser(accParser)
  }

  def allOperators: List[(String, Tree)] = groups.flatMap(group => group.operators)
}
