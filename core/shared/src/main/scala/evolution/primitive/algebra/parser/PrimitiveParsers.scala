package evolution.primitive.algebra.parser

import evolution.geometry.Point
import fastparse.all

trait PrimitiveParsers {
  import ParserConfig.White._
  import fastparse.noApi._

  val digit: Parser[Unit] =
    P(CharIn('0' to '9'))

  val floatDigits: all.Parser[Unit] =
    P(digit.rep ~ "." ~ digit.rep(1))

  val double: Parser[Double] =
    P("-".? ~ (floatDigits | digit.rep(1))).!.map(_.toDouble)

  val point: Parser[Point] =
    function2("point", double, double).map { case (x, y) => Point(x, y) }

  lazy val stringLiteral: Parser[String] =
    P("\"" ~/ CharIn('a' to 'z').rep.! ~/ "\"")

  val varName: Parser[String] = {
    val letter = P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn(Seq('_', '-')) | CharIn('0' to '9'))
    P(letter.rep(1).!)
  }

  def varUsage(varName: String): Parser[String] = P("$" ~ varName.!)

  def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
    P(funcName ~/ "(" ~ parser ~ ")")
  // TODO Remove the CUT to allow overloading. Find a solution to the problem
  def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
    P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ ")")
  def function3[A, B, C](
    funcName: String,
    parser1: Parser[A],
    parser2: Parser[B],
    parser3: Parser[C]
  ): Parser[(A, B, C)] =
    P(funcName ~ "(" ~/ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")

  /**
    * A function with two parameters list, where the second one can depend on the first
    */
  def functionFlatMap[A, B](func: Parser[A], f: A => Parser[B]): Parser[B] =
    func.flatMap(a => P("(" ~ f(a) ~ ")"))
  def prefix[A](operator: String, parser: Parser[A]): Parser[A] =
    P(operator ~ parser)
  def infix[A, B](operator: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
    P(parser1 ~ operator ~ parser2)
  def whitespaceWrap[T](p: Parser[T]): Parser[T] =
    P(ParserConfig.whitespaces ~ p ~ ParserConfig.whitespaces)
}

object PrimitiveParsers extends PrimitiveParsers
