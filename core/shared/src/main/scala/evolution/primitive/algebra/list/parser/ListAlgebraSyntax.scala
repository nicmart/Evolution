package evolution.primitive.algebra.list.parser
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.parser.PrimitiveParsers.function2
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._
import evolution.primitive.algebra.parser.PrimitiveParsers._
import fastparse.noApi.{P, Parser}

class ListAlgebraSyntax[S[_], F[_], R[_]](alg: ListAlgebra[S, F, R]) extends ListAlgebra[S, F, λ[α => Parser[R[α]]]] {

  override def empty[A]: Parser[R[F[A]]] =
    P("empty").map(_ => alg.empty)

  override def cons[A](head: Parser[R[S[A]]], tail: Parser[R[F[A]]]): Parser[R[F[A]]] =
    function2("cons", head, tail).map[R[F[A]]] { case (h, t) => alg.cons(h, t) }

  override def mapEmpty[A](eva: Parser[R[F[A]]])(eva2: Parser[R[F[A]]]): Parser[R[F[A]]] =
    function2("mapEmpty", eva, eva2)
      .map[R[F[A]]] { case (in, out) => alg.mapEmpty(in)(out) }

  override def mapCons[A, B](eva: Parser[R[F[A]]])(f: Parser[R[S[A] => F[A] => F[B]]]): Parser[R[F[B]]] =
    function2("mapCons", eva, f)
      .map[R[F[B]]] { case (in, out) => alg.mapCons(in)(out) }
}
