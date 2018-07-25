package evolution.primitive.algebra.parser

import fastparse.noApi._
import ParserConfig.White._

case class ExtensibleParser[C, T](leaf: Parser[T], composite: C => Parser[T]) {
  def expr(container: C): Parser[T] =
    P(leaf | composite(container))
  def mapLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[C, T] =
    ExtensibleParser(f(leaf), composite)
  def addLeaf(newLeaf: Parser[T]): ExtensibleParser[C, T] =
    mapLeaf(previousLeaf => previousLeaf | newLeaf)
  def addComposite(newComposite: C => Parser[T]): ExtensibleParser[C, T] =
    ExtensibleParser(leaf, e => P(composite(e) | newComposite(e)))
  def extendWith(other: ExtensibleParser[C, T]): ExtensibleParser[C, T] =
    addLeaf(other.leaf).addComposite(other.composite)
}

object ExtensibleParser {
  def fail[C, T]: ExtensibleParser[C, T] = ExtensibleParser(Fail, _ => Fail)
}
