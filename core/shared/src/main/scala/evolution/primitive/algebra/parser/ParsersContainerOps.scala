package evolution.primitive.algebra.parser

import evolution.data.HasValue
import fastparse.noApi.Parser

class ParsersContainerOps[C](container: C) {
  def extensibleParser[T, F[_]](implicit hasValue: HasValue[C, ExtensibleParser[C, F[T]]]): ExtensibleParser[C, F[T]] =
    hasValue.get(container)
  def withExtensibleParser[T, F[_]](
    parser: ExtensibleParser[C, F[T]]
  )(implicit hasValue: HasValue[C, ExtensibleParser[C, F[T]]]): C =
    hasValue.set(container, parser)
  def parser[T, F[_]](implicit hasValue: HasValue[C, ExtensibleParser[C, F[T]]]): Parser[F[T]] =
    hasValue.get(container).expr(container)
}

object ParsersContainerOps {
  implicit def ops[C](container: C): ParsersContainerOps[C] = new ParsersContainerOps[C](container)
}
