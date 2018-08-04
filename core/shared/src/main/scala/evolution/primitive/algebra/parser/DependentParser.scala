package evolution.primitive.algebra.parser

import fastparse.noApi._
import ParserConfig.White._
import evolution.data.HasValue

final case class DependentParser[C, A](parser: C => Parser[A]) {
  def map[B](f: A => B): DependentParser[C, B] = DependentParser(c => parser(c).map(f))
  def contramap[C2](f: C2 => C): DependentParser[C2, A] =
    DependentParser(f andThen parser)
  def mapParser[B](f: Parser[A] => Parser[B]): DependentParser[C, B] =
    DependentParser(parser andThen f)
  def or(other: DependentParser[C, A]): DependentParser[C, A] =
    DependentParser(c => P(parser(c) | other.parser(c)))
}

object DependentParser {
  def empty[C, T]: DependentParser[C, T] = DependentParser(_ => Fail)
  type HasParser[C, T] = HasValue[C, DependentParser[C, T]]
  object HasParser {
    def instance[C, T](_get: C => DependentParser[C, T], _set: (C, DependentParser[C, T]) => C): HasParser[C, T] =
      HasValue.instance(_get, _set)
  }
}
