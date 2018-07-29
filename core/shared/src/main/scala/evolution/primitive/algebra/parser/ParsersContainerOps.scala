package evolution.primitive.algebra.parser

import evolution.data.HasValue
import fastparse.noApi.Parser
import ParsersContainerOps._

class ParsersContainerOps[C](container: C) {
  def extensibleParser[T](implicit hasValue: HasValue[C, ExtensibleParser[C, T]]): ExtensibleParser[C, T] =
    hasValue.get(container)
  def withExtensibleParser[T](
    parser: ExtensibleParser[C, T]
  )(implicit hasValue: HasValue[C, ExtensibleParser[C, T]]): C =
    hasValue.set(container, parser)
  def addExtensibleParser[T](
    parser: ExtensibleParser[C, T]
  )(implicit hasValue: HasValue[C, ExtensibleParser[C, T]]): C =
    hasValue.set(container, container.extensibleParser[T].extendWith(parser))
  def parser[T](implicit hasValue: HasValue[C, ExtensibleParser[C, T]]): Parser[T] =
    hasValue.get(container).expr(container)
}

object ParsersContainerOps {
  implicit def ops[C](container: C): ParsersContainerOps[C] = new ParsersContainerOps[C](container)
}
