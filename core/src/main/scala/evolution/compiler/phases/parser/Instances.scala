/*
 * Copyright 2017-2021 Lenses.io Ltd
 */
package evolution.compiler.phases.parser

import cats.parse.Parser.With1
import evolution.compiler.phases.parser.CatsParserConfig.whitespaces

import cats.parse.{Parser => P, Parser0 => P0}

object Instances:
  implicit class POps[A](p: P[A]):
    def ~|[B](other: P0[B]): P[(A, B)] = (p ~ other).backtrack
    def ~~[B](other: P0[B]): P[(A, B)] = (p <* whitespaces.?) ~ other
    def w: P[A] = p <* whitespaces.?
    def surroundedByWhitespaces: P[A] = whitespaces.?.with1.soft *> p <* whitespaces.?

  implicit class P0Ops[A](p: P0[A]):
    def w: P0[A] = p <* whitespaces.?

  implicit class PObjOps[A](p: P.type):
    def charInPattern(pattern: String): P[Char] =
      p.charWhere(_.toString.matches(s"[$pattern]"))

  implicit class With1Ops[A](p: With1[A]):
    def ~|[B](other: P[B]): P[(A, B)] = (p ~ other).backtrack
