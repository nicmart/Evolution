package evolution.algebra.primitive.parser

import evolution.primitive.algebra.parser.PrimitiveParsers.function2
import evolution.primitive.algebra.parser.{ExtensibleParser, ParserConfig, PrimitiveParsers}

trait CommonTestParsers extends PrimitiveParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  def doubleParser[T](f: Double => T): Parser[T] = double.map(f)

  def extensibleDoubleParser[T](f: Double => T): ExtensibleParser[Unit, T] =
    ExtensibleParser.Leaf(doubleParser(f))

  def unaryOpParser[A, B](name: String, op: A => B)(innerParser: Parser[A]): Parser[B] =
    P(function1(name, innerParser).map(op))

  def extensibleUnaryOpParser[A, B](name: String, op: A => B): ExtensibleParser[Parser[A], B] =
    ExtensibleParser.Composite(unaryOpParser(name, op))

  def binaryOpParser[T](name: String, op: (T, T) => T)(innerParser: Parser[T]): Parser[T] =
    P(function2(name, innerParser, innerParser).map {
      case (expr1, expr2) =>
        op(expr1, expr2)
    })

  def extensibleBinaryOpParser[T](name: String, op: (T, T) => T): ExtensibleParser[Parser[T], T] =
    ExtensibleParser.Composite(binaryOpParser(name, op))

  def letParser[A, B](assignment: Parser[A], body: String => Parser[B], let: (String, A, B) => B): Parser[B] =
    functionFlatMap[(String, A), B](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => let(name, valueExpr, bodyExpr))
    })

  def unsafeParse[T](expression: String, parser: Parser[T]): T =
    parser.parse(expression).get.value

}
