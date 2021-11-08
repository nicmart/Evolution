/*
 * Copyright 2017-2021 Lenses.io Ltd
 */
package evolution.compiler.phases.parser

import cats.parse.Parser.With1
import evolution.compiler.phases.parser.CatsParserConfig.whitespaces
import evolution.compiler.tree.Pos

import cats.parse.{Parser => P, Parser0 => P0}

object Instances:
  extension [A](p: P[A])
    def ~|[B](other: P0[B]): P[(A, B)] = (p ~ other).backtrack
    def ~~[B](other: P0[B]): P[(A, B)] = (p <* whitespaces.?) ~ other
    def w: P[A] = p <* whitespaces.?
    def mapWithPos[B](f: (A, Pos) => B): P[B] = (P.index.with1 ~ p ~ P.index).map { case ((start, p), end) =>
      f(p, Pos(start, end))
    }
    def addPos: P[(A, Pos)] = mapWithPos((a, pos) => (a, pos))
    //def withPos: P[(A, Pos)] = mapWithPos((a, pos) => (a, pos))
    def onlyPos: P[Pos] = mapWithPos((a, pos) => pos)
    def surroundedByWhitespaces: P[A] = whitespaces.?.with1.soft *> p <* whitespaces.?

  extension [A](p: P0[A])
    def w: P0[A] = p <* whitespaces.?
    def mapWithPos[B](f: (A, Pos) => B): P0[B] = (P.index ~ p ~ P.index).map { case ((start, p), end) =>
      f(p, Pos(start, end))
    }
    def onlyPos: P0[Pos] = mapWithPos((a, pos) => pos)
    def addPos: P0[(A, Pos)] = mapWithPos((a, pos) => (a, pos))

  extension [A](p: P.type)
    def charInPattern(pattern: String): P[Char] =
      p.charWhere(_.toString.matches(s"[$pattern]"))

  extension [A](p: With1[A]) def ~|[B](other: P[B]): P[(A, B)] = (p ~ other).backtrack
