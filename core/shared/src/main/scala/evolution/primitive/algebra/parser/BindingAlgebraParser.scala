package evolution.primitive.algebra.parser

import evolution.geometry.Point
import evolution.primitive.algebra.parser.PrimitiveParsers.whitespaceWrap
import evolution.primitive.algebra.{BindingAlgebra, Type, TypeAlg}
import fastparse.all
import fastparse.noApi.Parser

class BindingAlgebraParser[F[_]](alg: BindingAlgebra[F]) {
  import ParserConfig.White._
  import fastparse.noApi._
  import PrimitiveParsers._

  def var0[A](varName: String): Parser[F[A]] =
    P("$" ~ varName ~ !varName).map(_ => alg.var0)

  def shift[A](current: Parser[F[A]]): Parser[F[A]] =
    current.map(fa => alg.shift(fa))

  def let[Out](original: Parser[F[Out]]): Parser[F[Out]] = {
    val lValueWithSyntaxParser = P("let" ~ "(" ~ varName ~ ",")
    val rValueWithSyntaxParser = P(let(original) ~ "," ~ "")

    original | P(
      letParser(lValueWithSyntaxParser, rValueWithSyntaxParser, name => let(let(original).map(alg.shift) | var0(name))) ~ ")"
    )
  }

  def expr[A](current: Parser[F[A]]): Parser[F[A]] = ???

  private def letParser[In, Out](
    lValueParser: Parser[String],
    rValueParser: Parser[F[In]],
    bodyParser: String => Parser[F[Out]]
  ): Parser[F[Out]] =
    P(lValueParser ~ rValueParser).flatMap {
      case (name, value) => whitespaceWrap(bodyParser(name).map(e => alg.let(name, value)(e)))
    }
}
