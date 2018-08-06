package evolution.primitive.algebra.parser

import evolution.primitive.algebra.CoreDrawingAlgebra
import ParsersContainerOps._
import PrimitiveParsers._
import evolution.primitive.algebra.parser.DependentParser.HasParser

class CoreDrawingAlgebraParser[S[_], F[_], R[_]](alg: CoreDrawingAlgebra[S, F, R]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer1[C, T](container: C)(
    implicit
    hasParserS: HasParser[C, R[S[T]]],
    hasParserF: HasParser[C, R[F[T]]]
  ): C =
    container
      .addParser(parser1[C, T])
      .addParser(parser2[C, T, T])

  def buildContainer2[C, T1, T2](container: C)(
    implicit
    hasParser1S: HasParser[C, R[S[T1]]],
    hasParser1F: HasParser[C, R[F[T1]]],
    hasParser2S: HasParser[C, R[S[T2]]],
    hasParser2F: HasParser[C, R[F[T2]]]
  ): C =
    container
      .addParser(parser2[C, T1, T1])
      .addParser(parser2[C, T1, T2])
      .addParser(parser2[C, T2, T1])
      .addParser(parser2[C, T2, T2])

  private def parser1[C, T](
    implicit
    hasParserS: HasParser[C, R[S[T]]],
    hasParserF: HasParser[C, R[F[T]]]
  ): DependentParser[C, R[F[T]]] =
    empty[C, T]
      .or(cons[C, T])
      .or(mapEmptyParser[C, T])

  private def parser2[C, T1, T2](
    implicit
    hasParser1S: HasParser[C, R[S[T1]]],
    hasParser1F: HasParser[C, R[F[T1]]],
    hasParser2S: HasParser[C, R[S[T2]]],
    hasParser2F: HasParser[C, R[F[T2]]]
  ): DependentParser[C, R[F[T2]]] =
    parser1[C, T2]
      .or(mapConsParser[C, T1, T2])
      .or(mapConsParser[C, T2, T2])

  private def mapConsParser[C, In, Out](
    implicit
    hasParserFIn: HasParser[C, R[F[In]]],
    hasParserFOut: HasParser[C, R[F[Out]]]
  ): DependentParser[C, R[F[Out]]] = {
    DependentParser(
      c =>
        function2("mapCons", c.parser[R[F[In]]], c.parser[R[F[Out]]])
        // TODO here we need a  function
          .map[R[F[Out]]] { case (in, out) => alg.mapCons(in)(???) }
    )
  }

  private def mapEmptyParser[C, T](
    implicit
    hasParserF: HasParser[C, R[F[T]]]
  ): DependentParser[C, R[F[T]]] = {
    DependentParser(
      c =>
        function2("mapEmpty", c.parser[R[F[T]]], c.parser[R[F[T]]])
          .map[R[F[T]]] { case (in, out) => alg.mapEmpty(in)(out) }
    )
  }

  private def empty[C, T]: DependentParser[C, R[F[T]]] = DependentParser(_ => P("empty").map(_ => alg.empty))

  private def cons[C, T](
    implicit hasParserS: HasParser[C, R[S[T]]],
    hasParserF: HasParser[C, R[F[T]]]
  ): DependentParser[C, R[F[T]]] = {
    DependentParser(
      c => function2("cons", c.parser[R[S[T]]], c.parser[R[F[T]]]).map[R[F[T]]] { case (h, t) => alg.cons(h, t) }
    )
  }
}
