package evolution.primitive.algebra.parser

import fastparse.noApi._
import ParserConfig.White._
import evolution.data.HasValue

case class ExtensibleParser[C, T](leaf: Parser[T], composite: C => Parser[T]) {
  def expr(container: C): Parser[T] =
    P(leaf | composite(container))
  def map[U](f: T => U): ExtensibleParser[C, U] =
    ExtensibleParser(leaf.map(f), c => composite(c).map(f))
  def contramap[C2](f: C2 => C): ExtensibleParser[C2, T] =
    ExtensibleParser(leaf, f andThen composite)
  def transformLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[C, T] =
    ExtensibleParser(f(leaf), composite)
  def addLeaf(newLeaf: Parser[T]): ExtensibleParser[C, T] =
    transformLeaf(previousLeaf => previousLeaf | newLeaf)
  def addComposite(newComposite: C => Parser[T]): ExtensibleParser[C, T] =
    ExtensibleParser(leaf, e => P(composite(e) | newComposite(e)))
  def extendWith(other: ExtensibleParser[C, T]): ExtensibleParser[C, T] =
    addLeaf(other.leaf).addComposite(other.composite)
}

object ExtensibleParser {
  type HasParser[C, T] = HasValue[C, ExtensibleParser[C, T]]
  object HasParser {
    def instance[C, T](_get: C => ExtensibleParser[C, T], _set: (C, ExtensibleParser[C, T]) => C): HasParser[C, T] =
      HasValue.instance(_get, _set)
  }
  def fail[C, T]: ExtensibleParser[C, T] = ExtensibleParser(Fail, _ => Fail)
}
