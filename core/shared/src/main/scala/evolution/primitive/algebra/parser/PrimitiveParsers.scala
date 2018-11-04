package evolution.primitive.algebra.parser

import fastparse.all

trait PrimitiveParsers {
  import ParserConfig.White._
  import fastparse.noApi._

  private val digit: Parser[Unit] =
    P(CharIn('0' to '9'))

  private val floatDigits: all.Parser[Unit] =
    P(digit.rep ~ "." ~ digit.rep(1))

  val doubleLiteral: Parser[Double] =
    P("-".? ~ (floatDigits | digit.rep(1))).!.map(_.toDouble)

  val varName: Parser[String] = {
    val letter = P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9'))
    P(letter.rep(1).!)
  }

  def varUsage(varName: String): Parser[String] = P("$" ~ varName.!)

  def WP[T](p: => Parser[T]): Parser[T] =
    P(ParserConfig.whitespaces ~ p ~ ParserConfig.whitespaces)
}

object PrimitiveParsers extends PrimitiveParsers
