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

case class ExtParser[T, F[_]](leaf: Parser[F[T]], composite: HOExtensibleParser[F] => Parser[F[T]]) {
  def mapLeaf(f: Parser[F[T]] => Parser[F[T]]): ExtParser[T, F] =
    ExtParser(f(leaf), composite)
  def addLeaf(newLeaf: Parser[F[T]]): ExtParser[T, F] =
    mapLeaf(previousLeaf => previousLeaf | newLeaf)
  def addComposite(newComposite: HOExtensibleParser[F] => Parser[F[T]]): ExtParser[T, F] =
    ExtParser(leaf, e => P(composite(e) | newComposite(e)))
  def extendWith(other: ExtParser[T, F]): ExtParser[T, F] =
    addLeaf(other.leaf).addComposite(other.composite)
}

case class HOExtensibleParser[F[_]](double: ExtParser[Double, F], string: ExtParser[String, F]) {
  def doubleExpr: Parser[F[Double]] = P(double.leaf | double.composite(this))
  def stringExpr: Parser[F[String]] = P(string.leaf | string.composite(this))
  def extendWith(parser: HOExtensibleParser[F]): HOExtensibleParser[F] =
    HOExtensibleParser(double.extendWith(parser.double), string.extendWith(parser.string))
}
