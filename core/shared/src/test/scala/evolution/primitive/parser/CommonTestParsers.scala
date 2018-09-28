package evolution.primitive.parser

import evolution.primitive.algebra.parser.PrimitiveParsers.function2
import evolution.primitive.algebra.parser.{DependentParser, ParserConfig, PrimitiveParsers}

trait CommonTestParsers extends PrimitiveParsers {
  import ParserConfig.White._
  import fastparse.noApi._

  def doubleParser[T](f: Double => T): Parser[T] = double.map(f)

  def dependentDoubleParser[T](f: Double => T): DependentParser[Unit, T] =
    DependentParser(_ => doubleParser(f))

  def unaryOpParser[A, B](name: String, op: A => B)(innerParser: Parser[A]): Parser[B] =
    P(function1(name, innerParser).map(op))

  def dependentUnaryOpParser[A, B](name: String, op: A => B): DependentParser[Parser[A], B] =
    DependentParser(unaryOpParser[A, B](name, op))

  def binaryOpParser[T](name: String, op: (T, T) => T)(innerParser: Parser[T]): Parser[T] =
    P(function2(name, innerParser, innerParser).map {
      case (expr1, expr2) =>
        op(expr1, expr2)
    })

  def dependentBinaryOpParser[T](name: String, op: (T, T) => T): DependentParser[Parser[T], T] =
    DependentParser(binaryOpParser(name, op))

  def letParser[A, B](assignment: Parser[A], body: String => Parser[B], let: (String, A, B) => B): Parser[B] =
    functionFlatMap[(String, A), B](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => let(name, valueExpr, bodyExpr))
    })

  def unsafeParse[T](expression: String, parser: Parser[T]): T =
    parser.parse(expression).get.value

}
