/*
 * Copyright 2017-2021 Lenses.io Ltd
 */
package evolution.compiler.phases.parser

import evolution.compiler.tree.Tree
import cats.parse.{Parser as P}

private[parser] final case class PrecedenceGroups(last: P[Tree], groups: List[PrecedenceGroup]):
  def operand: P[Tree] =
    groups.foldRight(last) { (group, accParser) =>
      group.parser(accParser)
    }
