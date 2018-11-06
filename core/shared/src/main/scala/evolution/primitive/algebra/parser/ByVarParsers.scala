package evolution.primitive.algebra.parser
import cats.Id
import evolution.primitive.algebra.parser.ByVarParser.{Prefixed, Raw}
import fastparse.noApi.{P, Parser}
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

object ByVarParsers {

  val doubleLiteral: ByVarParser[Double] =
    Raw(_ => PrimitiveParsers.doubleLiteral)

  def function1[A](funcName: String, parser: ByVarParser[A]): ByVarParser[A] =
    Prefixed(funcName, Raw(vars => P("(" ~ parser.parser(vars) ~ ")")))

  def function2[A, B](funcName: String, parser1: ByVarParser[A], parser2: ByVarParser[B]): ByVarParser[(A, B)] =
    Prefixed(funcName, Raw(vars => P("(" ~ parser1.parser(vars) ~ "," ~ parser2.parser(vars) ~ ")")))

  def function3Dep[A, B, C](
    funcName: String,
    parser1: ByVarParser[A],
    parser2: A => ByVarParser[B],
    parser3: (A, B) => ByVarParser[C]
  ): ByVarParser[(A, B, C)] =
    Prefixed(funcName, Raw { vars =>
      for {
        a <- PrimitiveParsers.WP("(" ~ parser1.parser(vars) ~ ",")
        b <- PrimitiveParsers.WP(parser2(a).parser(vars) ~ ",")
        c <- PrimitiveParsers.WP(parser3(a, b).parser(vars) ~ ")")
      } yield (a, b, c)
    })

  def infixFlatMap[A, B](parser1: ByVarParser[A], operator: String, parser2: A => ByVarParser[B]): ByVarParser[B] =
    Raw { vars =>
      parser1.parser(vars).flatMap { a =>
        P(PrimitiveParsers.WP(operator) ~ parser2(a).parser(vars))
      }
    }
}
