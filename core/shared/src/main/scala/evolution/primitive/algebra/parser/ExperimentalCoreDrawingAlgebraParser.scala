package evolution.primitive.algebra.parser

import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser.ParsersContainerOps._

import scala.collection.immutable

class ExperimentalCoreDrawingAlgebraParser[S[_], F[_], R[_]](alg: CoreDrawingAlgebra[S, F, R]) {
  final type RF[T] = R[F[T]]
  final type RS[T] = R[S[T]]
  import ParserConfig.White._
  import PrimitiveParsers._
  import fastparse.noApi._

  def parserSet[A]: ParserSet.Aux[A] = new ParserSet {
    override type T = A
    override def all: List[ParserSet] = ???
  }

  private def mapConsParser[C, In, Out](
    inParser: => DependentParser[C, RF[In]],
    fParser: => DependentParser[C, R[S[In] => F[In] => F[Out]]]
  ): DependentParser[C, R[F[Out]]] =
    DependentParser(
      c =>
        function2("mapCons", inParser.parser(c), fParser.parser(c))
          .map[R[F[Out]]] { case (in, out) => alg.mapCons(in)(out) }
    )

  private def mapEmptyParser[C, T](parser: DependentParser[C, RF[T]]): DependentParser[C, R[F[T]]] = {
    DependentParser(
      c =>
        function2("mapEmpty", parser.parser(c), parser.parser(c))
          .map[R[F[T]]] { case (in, out) => alg.mapEmpty(in)(out) }
    )
  }

  private def empty[C, T]: DependentParser[C, R[F[T]]] = DependentParser(_ => P("empty").map(_ => alg.empty))

  private def cons[C, T](
    head: DependentParser[C, RS[T]],
    tail: DependentParser[C, RF[T]]
  ): DependentParser[C, R[F[T]]] = {
    DependentParser(
      c => function2("cons", head.parser(c), tail.parser(c)).map[R[F[T]]] { case (h, t) => alg.cons(h, t) }
    )
  }

  trait ParserSet {
    type T
    def all: List[ParserSet]

    def parserF: DependentParser[ParserSet, RF[T]] =
      empty
        .or(cons(parserS, parserF))
        .or(mapEmptyParser(parserF))
        .or(combinedMapConsParser)

    def parserS: DependentParser[ParserSet, RS[T]] =
      DependentParser.empty

    def parserFunc(other: ParserSet): DependentParser[ParserSet, R[S[other.T] => F[other.T] => F[T]]] =
      DependentParser.empty

    private def combinedMapConsParser: DependentParser[ParserSet, RF[T]] = {
      val allMapConsParsers: List[DependentParser[ParserSet, RF[T]]] = for {
        parserSet <- all
        parserForFunction = parserFunc(parserSet)
        parserForMapCons = mapConsParser(parserSet.parserF, parserForFunction)
      } yield parserForMapCons

      allMapConsParsers.reduceLeft(_.or(_))
    }
  }

  object ParserSet {
    type Aux[X] = ParserSet { type T = X }
  }
}
