package evolution.primitive.algebra.parser

import fastparse.noApi._
import fastparse.all
import ParserConfig.White._

case class ExtensibleParser[T](leaf: Parser[T], composite: ExtensibleParser[T] => Parser[T]) {
  def expr: Parser[T] = P(leaf | composite(this))
  def mapLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[T] =
    ExtensibleParser(f(leaf), composite)
  def addLeaf(newLeaf: Parser[T]): ExtensibleParser[T] =
    mapLeaf(previousLeaf => previousLeaf | newLeaf)
  def addComposite(newComposite: ExtensibleParser[T] => Parser[T]): ExtensibleParser[T] =
    ExtensibleParser(leaf, e => P(composite(e) | newComposite(e)))
  def extendWith(other: ExtensibleParser[T]): ExtensibleParser[T] =
    addLeaf(other.leaf).addComposite(other.composite)
}

object ExtensibleParser {
  def apply[T](leaf: Parser[T]): ExtensibleParser[T] =
    ExtensibleParser(leaf, _ => Fail)
}

case class ExtParser[C, T](leaf: Parser[T], composite: C => Parser[T]) {
  def mapLeaf(f: Parser[T] => Parser[T]): ExtParser[C, T] =
    ExtParser(f(leaf), composite)
  def addLeaf(newLeaf: Parser[T]): ExtParser[C, T] =
    mapLeaf(previousLeaf => previousLeaf | newLeaf)
  def addComposite(newComposite: C => Parser[T]): ExtParser[C, T] =
    ExtParser(leaf, e => P(composite(e) | newComposite(e)))
  def extendWith(other: ExtParser[C, T]): ExtParser[C, T] =
    addLeaf(other.leaf).addComposite(other.composite)
}
