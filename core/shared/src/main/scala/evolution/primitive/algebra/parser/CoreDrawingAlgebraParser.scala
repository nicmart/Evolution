package evolution.primitive.algebra.parser

import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser.ExtensibleParser.HasParser
import ParsersContainerOps._
import PrimitiveParsers._

class CoreDrawingAlgebraParser[S[_], F[_]](alg: CoreDrawingAlgebra[S, F]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer1[C, T](container: C)(
    implicit
    hasParserS: HasParser[C, S[T]],
    hasParserF: HasParser[C, F[T]]
  ): C =
    container
      .addExtensibleParser(parser1[C, T])
      .addExtensibleParser(parser2[C, T, T])

  def buildContainer2[C, T1, T2](container: C)(
    implicit
    hasParser1S: HasParser[C, S[T1]],
    hasParser1F: HasParser[C, F[T1]],
    hasParser2S: HasParser[C, S[T2]],
    hasParser2F: HasParser[C, F[T2]]
  ): C =
    container
      .addExtensibleParser(parser2[C, T1, T1])
      .addExtensibleParser(parser2[C, T1, T2])
      .addExtensibleParser(parser2[C, T2, T1])
      .addExtensibleParser(parser2[C, T2, T2])

  private def parser1[C, T](
    implicit
    hasParserS: HasParser[C, S[T]],
    hasParserF: HasParser[C, F[T]]
  ): ExtensibleParser[C, F[T]] =
    empty[C, T]
      .extendWith(cons[C, T])
      .extendWith(mapEmptyParser[C, T])

  private def parser2[C, T1, T2](
    implicit
    hasParser1S: HasParser[C, S[T1]],
    hasParser1F: HasParser[C, F[T1]],
    hasParser2S: HasParser[C, S[T2]],
    hasParser2F: HasParser[C, F[T2]]
  ): ExtensibleParser[C, F[T2]] =
    parser1[C, T2]
      .extendWith(mapConsParser[C, T1, T2])
      .extendWith(mapConsParser[C, T2, T2])

  private def mapConsParser[C, In, Out](
    implicit
    hasParserFIn: HasParser[C, F[In]],
    hasParserFOut: HasParser[C, F[Out]]
  ): ExtensibleParser[C, F[Out]] = {
    ExtensibleParser.Composite(
      c =>
        function2("mapCons", c.parser[F[In]], c.parser[F[Out]])
          .map[F[Out]] { case (in, out) => alg.mapCons(in)(out) }
    )
  }

  private def mapEmptyParser[C, T](
    implicit
    hasParserF: HasParser[C, F[T]]
  ): ExtensibleParser[C, F[T]] = {
    ExtensibleParser.Composite(
      c =>
        function2("mapEmpty", c.parser[F[T]], c.parser[F[T]])
          .map[F[T]] { case (in, out) => alg.mapEmpty(in)(out) }
    )
  }

  private def empty[C, T]: ExtensibleParser[C, F[T]] = ExtensibleParser.Leaf(P("empty").map(_ => alg.empty))

  private def cons[C, T](
    implicit hasParserS: HasParser[C, S[T]],
    hasParserF: HasParser[C, F[T]]
  ): ExtensibleParser[C, F[T]] = {
    ExtensibleParser.Composite(
      c => function2("cons", c.parser[S[T]], c.parser[F[T]]).map[F[T]] { case (h, t) => alg.cons(h, t) }
    )
  }
}
