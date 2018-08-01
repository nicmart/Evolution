package evolution.primitive.algebra.parser

import fastparse.noApi._
import ParserConfig.White._
import evolution.data.HasValue
import evolution.primitive.algebra.parser.ExtensibleParser.Composite
import fastparse.noApi
import ExtensibleParser._

//TODO Can we find a way to avoid to thousands of Fail parsers? Some ADT?
sealed trait ExtensibleParser[C, T] extends ExtensibleParserOps[C, T] {
  def leaf: Parser[T]
  def composite(c: C): Parser[T]
  def hasLeaf: Boolean
  def hasComposite: Boolean
  def map[U](f: T => U): ExtensibleParser[C, U]
  def contramap[C2](f: C2 => C): ExtensibleParser[C2, T]
  def transformLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[C, T]
}

trait ExtensibleParserOps[C, T] { self: ExtensibleParser[C, T] =>
  def expr(container: C): Parser[T] =
    P(leaf | composite(container))

  def addLeaf(newLeaf: Parser[T]): ExtensibleParser[C, T] =
    Or(this, Leaf(newLeaf))

  def addComposite(newComposite: C => Parser[T]): ExtensibleParser[C, T] =
    Or(this, Composite(newComposite))

  def extendWith(other: ExtensibleParser[C, T]): ExtensibleParser[C, T] =
    addLeaf(other.leaf).addComposite(other.composite)
}

object ExtensibleParser {
  final case class Empty[C, T]() extends ExtensibleParser[C, T] {
    override def leaf: Parser[Nothing] = Fail
    override def composite(c: C): noApi.Parser[Nothing] = Fail
    override def hasLeaf: Boolean = false
    override def hasComposite: Boolean = false
    override def map[U](f: T => U): ExtensibleParser[C, U] = Empty[C, U]()
    override def contramap[C2](f: C2 => C): ExtensibleParser[C2, T] = Empty[C2, T]()
    override def transformLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[C, T] = this
  }

  final case class Leaf[C, T](parser: Parser[T]) extends ExtensibleParser[C, T] {
    override def leaf: Parser[T] = parser
    override def composite(c: C): Parser[T] = Fail
    override def hasLeaf: Boolean = true
    override def hasComposite: Boolean = false
    override def map[U](f: T => U): ExtensibleParser[C, U] = super.map(f)
    override def contramap[C2](f: C2 => C): ExtensibleParser[C2, T] = Leaf(parser)
    override def transformLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[C, T] = Leaf(f(parser))
  }

  final case class Composite[C, T](parser: C => Parser[T]) extends ExtensibleParser[C, T] {
    override def leaf: Parser[T] = Fail
    override def composite(c: C): Parser[T] = parser(c)
    override def hasLeaf: Boolean = false
    override def hasComposite: Boolean = true
    override def map[U](f: T => U): ExtensibleParser[C, U] = Composite(c => parser(c).map(f))
    override def contramap[C2](f: C2 => C): ExtensibleParser[C2, T] = Composite(f andThen parser)
    override def transformLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[C, T] = this
  }

  final case class Or[C, T](extensibleParser1: ExtensibleParser[C, T], extensibleParser2: ExtensibleParser[C, T])
      extends ExtensibleParser[C, T] {
    override def leaf: Parser[T] = (extensibleParser1.hasLeaf, extensibleParser2.hasLeaf) match {
      case (true, true) => P(extensibleParser1.leaf | extensibleParser2.leaf)
      case (true, false) => extensibleParser1.leaf
      case (false, true) => extensibleParser2.leaf
      case _ => Fail
    }

    override def composite(c: C): Parser[T] =
      (extensibleParser1.hasComposite, extensibleParser2.hasComposite) match {
        case (true, true) => P(extensibleParser1.composite(c) | extensibleParser2.composite(c))
        case (true, false) => extensibleParser1.composite(c)
        case (false, true) => extensibleParser2.composite(c)
        case _ => Fail
      }
    override def hasLeaf: Boolean = extensibleParser1.hasLeaf || extensibleParser2.hasLeaf
    override def hasComposite: Boolean = extensibleParser1.hasComposite || extensibleParser2.hasComposite
    override def map[U](f: T => U): ExtensibleParser[C, U] = Or(extensibleParser1.map(f), extensibleParser2.map(f))
    override def contramap[C2](f: C2 => C): ExtensibleParser[C2, T] =
      Or(extensibleParser1.contramap(f), extensibleParser2.contramap(f))
    override def transformLeaf(f: Parser[T] => Parser[T]): ExtensibleParser[C, T] =
      Or(extensibleParser1.transformLeaf(f), extensibleParser2.transformLeaf(f))
  }

  def apply[C, T](leaf: Parser[T], composite: C => Parser[T]): ExtensibleParser[C, T] =
    Or(Leaf(leaf), Composite(composite))

  type HasParser[C, T] = HasValue[C, ExtensibleParser[C, T]]
  object HasParser {
    def instance[C, T](_get: C => ExtensibleParser[C, T], _set: (C, ExtensibleParser[C, T]) => C): HasParser[C, T] =
      HasValue.instance(_get, _set)
  }
}
