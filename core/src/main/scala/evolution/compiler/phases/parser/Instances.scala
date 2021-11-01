/*
 * Copyright 2017-2021 Lenses.io Ltd
 */
package evolution.compiler.phases.parser

import cats.parse.Parser.With1
import evolution.compiler.phases.parser.CatsParserConfig.whitespaces

import cats.parse.{Parser => P, Parser0 => P0}

object Instances:
  extension [A](p: P[A])
    def ~|[B](other: P0[B]): P[(A, B)] = (p ~ other).backtrack
    def ~~[B](other: P0[B]): P[(A, B)] = (p <* whitespaces.?) ~ other
    def w: P[A] = p <* whitespaces.?
    def surroundedByWhitespaces: P[A] = whitespaces.?.with1.soft *> p <* whitespaces.?

  extension [A](p: P0[A]) def w: P0[A] = p <* whitespaces.?

  extension [A](p: P.type)
    def charInPattern(pattern: String): P[Char] =
      p.charWhere(_.toString.matches(s"[$pattern]"))

  extension [A](p: With1[A]) def ~|[B](other: P[B]): P[(A, B)] = (p ~ other).backtrack
