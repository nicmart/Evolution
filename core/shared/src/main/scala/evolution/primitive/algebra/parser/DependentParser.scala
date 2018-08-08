package evolution.primitive.algebra.parser

import fastparse.noApi._
import ParserConfig.White._

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
}

trait HasParser[C, F[_], T] {
  def get(c: C): DependentParser[C, F[T]]
  def set(c: C, parser: DependentParser[C, F[T]]): C
}

object HasParser {
  def instance[C, F[_], T](
    _get: C => DependentParser[C, F[T]],
    _set: (C, DependentParser[C, F[T]]) => C
  ): HasParser[C, F, T] =
    new HasParser[C, F, T] {
      override def get(c: C): DependentParser[C, F[T]] = _get(c)
      override def set(c: C, parser: DependentParser[C, F[T]]): C = _set(c, parser)
    }

  object PushRight {
    implicit def pushRight[C, F1[_], F2[_], T](
      implicit p1: HasParser[C, Lambda[X => F1[F2[X]]], T]
    ): HasParser[C, F1, F2[T]] =
      HasParser.instance[C, F1, F2[T]](p1.get, p1.set)
  }
}
