package evolution.primitive.algebra.parser

import evolution.primitive.algebra.CoreDrawingAlgebra
import ParsersContainerOps._
import PrimitiveParsers._

class CoreDrawingAlgebraParser[S[_], F[_], R[_]](alg: CoreDrawingAlgebra[S, F, R]) {
  final type RF[T] = R[F[T]]
  final type RS[T] = R[S[T]]
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def buildContainer1[C, T](container: C)(
    implicit
    hasParserS: HasParser[C, RS, T],
    hasParserF: HasParser[C, RF, T]
  ): C =
    container
      .addParser[RF, T](parser1[C, T])
      .addParser[RF, T](parser2[C, T, T])

  def buildContainer2[C, T1, T2](container: C)(
    implicit
    hasParser1S: HasParser[C, RS, T1],
    hasParser1F: HasParser[C, RF, T1],
    hasParser2S: HasParser[C, RS, T2],
    hasParser2F: HasParser[C, RF, T2]
  ): C =
    container
      .addParser[RF, T1](parser2[C, T1, T1])
      .addParser[RF, T2](parser2[C, T1, T2])
      .addParser[RF, T1](parser2[C, T2, T1])
      .addParser[RF, T2](parser2[C, T2, T2])

  private def parser1[C, T](
    implicit
    hasParserS: HasParser[C, RS, T],
    hasParserF: HasParser[C, RF, T]
  ): DependentParser[C, R[F[T]]] =
    empty[C, T]
      .or(cons[C, T])
      .or(mapEmptyParser[C, T])

  private def parser2[C, T1, T2](
    implicit
    hasParser1S: HasParser[C, RS, T1],
    hasParser1F: HasParser[C, RF, T1],
    hasParser2S: HasParser[C, RS, T2],
    hasParser2F: HasParser[C, RF, T2]
  ): DependentParser[C, R[F[T2]]] =
    parser1[C, T2]
      .or(mapConsParser[C, T1, T2])
      .or(mapConsParser[C, T2, T2])

  private def mapConsParser[C, In, Out](
    implicit
    hasParserFIn: HasParser[C, RF, In],
    hasParserFOut: HasParser[C, RF, Out]
  ): DependentParser[C, R[F[Out]]] = {
    DependentParser(
      c =>
        function2("mapCons", c.parser[RF, In], c.parser[RF, Out])
        // TODO here we need a  function
          .map[R[F[Out]]] { case (in, out) => alg.mapCons(in)(???) }
    )
  }

  private def mapEmptyParser[C, T](
    implicit
    hasParserF: HasParser[C, RF, T]
  ): DependentParser[C, R[F[T]]] = {
    DependentParser(
      c =>
        function2("mapEmpty", c.parser[RF, T], c.parser[RF, T])
          .map[R[F[T]]] { case (in, out) => alg.mapEmpty(in)(out) }
    )
  }

  private def empty[C, T]: DependentParser[C, R[F[T]]] = DependentParser(_ => P("empty").map(_ => alg.empty))

  private def cons[C, T](
    implicit hasParserS: HasParser[C, RS, T],
    hasParserF: HasParser[C, RF, T]
  ): DependentParser[C, R[F[T]]] = {
    DependentParser(
      c => function2("cons", c.parser[RS, T], c.parser[RF, T]).map[R[F[T]]] { case (h, t) => alg.cons(h, t) }
    )
  }
}
