package evolution.compiler.phases.parsing

import fastparse._
import evolution.compiler.tree.TreeF.Tree

case class PrecedenceGroups(last: () => P[Tree], groups: List[PrecedenceGroup]) {
  def operand[_: P]: P[Tree] = {
    val lazyParser = groups.foldRight(last) { (group, accParser) => () =>
      group.parser(accParser)
    }
    lazyParser()
  }
}
