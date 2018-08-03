package evolution.primitive.algebra.parser

import evolution.primitive.algebra.CoreDrawingAlgebra
import ParsersContainerOps._
import PrimitiveParsers._
import evolution.primitive.algebra.parser.DependentParser.HasParser

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
      .addParser(parser1[C, T])
      .addParser(parser2[C, T, T])

  def buildContainer2[C, T1, T2](container: C)(
    implicit
    hasParser1S: HasParser[C, S[T1]],
    hasParser1F: HasParser[C, F[T1]],
    hasParser2S: HasParser[C, S[T2]],
    hasParser2F: HasParser[C, F[T2]]
  ): C =
    container
      .addParser(parser2[C, T1, T1])
      .addParser(parser2[C, T1, T2])
      .addParser(parser2[C, T2, T1])
      .addParser(parser2[C, T2, T2])

  private def parser1[C, T](
    implicit
    hasParserS: HasParser[C, S[T]],
    hasParserF: HasParser[C, F[T]]
  ): DependentParser[C, F[T]] =
    empty[C, T]
      .or(cons[C, T])
      .or(mapEmptyParser[C, T])

  private def parser2[C, T1, T2](
    implicit
    hasParser1S: HasParser[C, S[T1]],
    hasParser1F: HasParser[C, F[T1]],
    hasParser2S: HasParser[C, S[T2]],
    hasParser2F: HasParser[C, F[T2]]
  ): DependentParser[C, F[T2]] =
    parser1[C, T2]
      .or(mapConsParser[C, T1, T2])
      .or(mapConsParser[C, T2, T2])

  private def mapConsParser[C, In, Out](
    implicit
    hasParserFIn: HasParser[C, F[In]],
    hasParserFOut: HasParser[C, F[Out]]
  ): DependentParser[C, F[Out]] = {
    DependentParser(
      c =>
        function2("mapCons", c.parser[F[In]], c.parser[F[Out]])
          .map[F[Out]] { case (in, out) => alg.mapCons(in)(out) }
    )
  }

  private def mapEmptyParser[C, T](
    implicit
    hasParserF: HasParser[C, F[T]]
  ): DependentParser[C, F[T]] = {
    DependentParser(
      c =>
        function2("mapEmpty", c.parser[F[T]], c.parser[F[T]])
          .map[F[T]] { case (in, out) => alg.mapEmpty(in)(out) }
    )
  }

  private def empty[C, T]: DependentParser[C, F[T]] = DependentParser(_ => P("empty").map(_ => alg.empty))

  private def cons[C, T](
    implicit hasParserS: HasParser[C, S[T]],
    hasParserF: HasParser[C, F[T]]
  ): DependentParser[C, F[T]] = {
    DependentParser(c => function2("cons", c.parser[S[T]], c.parser[F[T]]).map[F[T]] { case (h, t) => alg.cons(h, t) })
  }
}
