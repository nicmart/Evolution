package evolution.primitive.algebra.chain.parser
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.parser.ByVarParser
import evolution.primitive.algebra.parser.ByVarParser.{ByVarParserK, Raw}
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._
import evolution.primitive.algebra.parser.ByVarParsers._
import fastparse.noApi.{P, Parser}

class ChainParserSyntax[F[_], R[_]](alg: Chain[F, R]) extends Chain[F, ByVarParserK[R, ?]] {

  override def empty[A]: ByVarParser[R[F[A]]] =
    Raw(_ => P("empty").map(_ => alg.empty), "empty")

  override def cons[A](head: ByVarParser[R[A]], tail: ByVarParser[R[F[A]]]): ByVarParser[R[F[A]]] =
    function2("cons", head, tail).map[R[F[A]]] { case (h, t) => alg.cons(h, t) }

  override def mapEmpty[A](eva: ByVarParser[R[F[A]]], eva2: ByVarParser[R[F[A]]]): ByVarParser[R[F[A]]] =
    function2("mapEmpty", eva, eva2)
      .map[R[F[A]]] { case (in, out) => alg.mapEmpty(in, out) }

  override def mapCons[A, B](eva: ByVarParser[R[F[A]]])(f: ByVarParser[R[A => F[A] => F[B]]]): ByVarParser[R[F[B]]] =
    function2("mapCons", eva, f)
      .map[R[F[B]]] { case (in, out) => alg.mapCons(in)(out) }
}
