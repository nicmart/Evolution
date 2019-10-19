package evolution.compiler.phases.parser

import evolution.compiler.tree.Tree
import fastparse._

case class PrecedenceGroups(last: () => P[Tree], groups: List[PrecedenceGroup]) {
  def operand[_: P]: P[Tree] = {
    val lazyParser = groups.foldRight(last) { (group, accParser) => () =>
      group.parser(accParser)
    }
    lazyParser()
  }
}
